package com.IntegrityTechnologies.business_manager.modules.person.entity.department.service;

import com.IntegrityTechnologies.business_manager.common.bulk.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.dto.BranchMinimalDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.dto.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.mapper.DepartmentMapper;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.dto.MinimalUserDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalTime;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class DepartmentBulkService {

    private final DepartmentService departmentService;
    private final DepartmentRepository departmentRepository;
    private final BranchRepository branchRepository;
    private final UserRepository userRepository;
    private final DepartmentMapper departmentMapper;

    /**
     * ATOMIC BULK IMPORT
     *
     * dryRun = true  → validate + preview (partial success allowed)
     * dryRun = false → ALL OR NOTHING (any error aborts entire import)
     */
    @Transactional
    public BulkResult<DepartmentDTO> importDepartments(
            BulkRequest<DepartmentBulkRow> request,
            Authentication authentication
    ) {

        BulkResult<DepartmentDTO> result = new BulkResult<>();
        result.setTotal(request.getItems().size());

        BulkOptions options =
                request.getOptions() != null
                        ? request.getOptions()
                        : new BulkOptions();

        /* ============================================================
           PHASE 1 — VALIDATION + PREPARATION (NO DB WRITES)
        ============================================================ */

        Set<String> seenNames = new HashSet<>();
        List<PreparedRow> preparedRows = new ArrayList<>();

        for (int i = 0; i < request.getItems().size(); i++) {
            int rowNum = i + 1;
            DepartmentBulkRow row = request.getItems().get(i);

            try {
                validate(row);

                String name = row.getName().trim();

                // In-file duplicate check
                if (!seenNames.add(name.toLowerCase())) {
                    throw new IllegalArgumentException(
                            "Duplicate department name in import: " + name
                    );
                }

                // Resolve branches (by code)
                Set<UUID> branchIds =
                        row.getBranchCodes().stream()
                                .map(code ->
                                        branchRepository.findByBranchCode(code.trim())
                                                .orElseThrow(() ->
                                                        new IllegalArgumentException(
                                                                "Unknown branchCode: " + code
                                                        )
                                                ).getId()
                                )
                                .collect(Collectors.toSet());

                // Resolve users
                Set<UUID> headIds = resolveUsers(row.getHeadUsernames());
                Set<UUID> memberIds = resolveUsers(row.getMemberUsernames());

                // Overlap check
                Set<UUID> overlap = new HashSet<>(headIds);
                overlap.retainAll(memberIds);
                if (!overlap.isEmpty()) {
                    String users = overlap.stream()
                            .map(id ->
                                    userRepository.findById(id)
                                            .map(User::getUsername)
                                            .orElse(id.toString())
                            )
                            .collect(Collectors.joining(", "));
                    throw new IllegalArgumentException(
                            "Users cannot be both heads and members: " + users
                    );
                }

                DepartmentDTO dto = new DepartmentDTO();
                dto.setName(name);
                dto.setDescription(row.getDescription());
                dto.setBranchIds(branchIds);
                dto.setHeadIds(headIds);
                dto.setMemberIds(memberIds);

                if (row.getRollcallStartTime() != null
                        && !row.getRollcallStartTime().isBlank()) {
                    try {
                        dto.setRollcallStartTime(
                                LocalTime.parse(row.getRollcallStartTime())
                        );
                    } catch (Exception ex) {
                        throw new IllegalArgumentException(
                                "Invalid rollcallStartTime (HH:mm): "
                                        + row.getRollcallStartTime()
                        );
                    }
                }

                dto.setGracePeriodMinutes(row.getGracePeriodMinutes());

                preparedRows.add(new PreparedRow(rowNum, dto));

            } catch (Exception ex) {
                result.addError(rowNum, ex.getMessage());
            }
        }

        /* ============================================================
           DRY RUN → RETURN PREVIEW (PARTIAL OK)
        ============================================================ */

        if (options.isDryRun()) {
            preparedRows.forEach(pr ->
                    result.addSuccess(buildPreviewDTO(pr.dto()))
            );
            return result;
        }

        /* ============================================================
           REAL IMPORT → ANY ERROR = TOTAL FAILURE
        ============================================================ */

        if (result.getFailed() > 0) {
            throw new BulkAbortException(result);
        }

        /* ============================================================
           PHASE 2 — PERSIST (SINGLE TRANSACTION)
        ============================================================ */

        for (PreparedRow pr : preparedRows) {

            DepartmentDTO dto = pr.dto();

            Optional<Department> existing =
                    departmentRepository.findByNameIgnoreCase(dto.getName());

            DepartmentDTO saved;

            if (existing.isPresent()) {

                if (!options.isUpdateExisting()) {
                    throw new IllegalArgumentException(
                            "Department already exists: " + dto.getName()
                    );
                }

                saved = departmentMapper.toDTO(
                        departmentService.updateExisting(
                                existing.get(),
                                dto,
                                authentication
                        )
                );

            } else {
                saved =
                        departmentService.create(
                                dto,
                                authentication
                        );
            }

            result.addSuccess(saved);
        }

        return result;
    }

    /* ============================================================
       PREVIEW HYDRATION (FOR DRY RUN)
    ============================================================ */

    private DepartmentDTO buildPreviewDTO(DepartmentDTO dto) {

        DepartmentDTO preview = new DepartmentDTO();
        preview.setName(dto.getName());
        preview.setDescription(dto.getDescription());
        preview.setRollcallStartTime(dto.getRollcallStartTime());
        preview.setGracePeriodMinutes(dto.getGracePeriodMinutes());

        if (dto.getBranchIds() != null) {
            preview.setBranches(
                    dto.getBranchIds().stream()
                            .map(id -> branchRepository.findById(id).orElse(null))
                            .filter(Objects::nonNull)
                            .map(b -> new BranchMinimalDTO(
                                    b.getId(),
                                    b.getBranchCode(),
                                    b.getName()
                            ))
                            .collect(Collectors.toSet())
            );
        }

        if (dto.getHeadIds() != null) {
            preview.setHeads(
                    dto.getHeadIds().stream()
                            .map(id -> userRepository.findById(id).orElse(null))
                            .filter(Objects::nonNull)
                            .map(u -> new MinimalUserDTO(
                                    u.getId(),
                                    u.getUsername()
                            ))
                            .collect(Collectors.toSet())
            );
        }

        if (dto.getMemberIds() != null) {
            preview.setMembers(
                    dto.getMemberIds().stream()
                            .map(id -> userRepository.findById(id).orElse(null))
                            .filter(Objects::nonNull)
                            .map(u -> new MinimalUserDTO(
                                    u.getId(),
                                    u.getUsername()
                            ))
                            .collect(Collectors.toSet())
            );
        }

        return preview;
    }

    /* ============================================================
       VALIDATION
    ============================================================ */

    private void validate(DepartmentBulkRow row) {
        if (row.getName() == null || row.getName().isBlank()) {
            throw new IllegalArgumentException("name is required");
        }

        if (row.getBranchCodes() == null || row.getBranchCodes().isEmpty()) {
            throw new IllegalArgumentException(
                    "At least one branchCode is required"
            );
        }
    }

    /* ============================================================
       USER RESOLUTION
    ============================================================ */

    private Set<UUID> resolveUsers(Set<String> usernames) {
        if (usernames == null || usernames.isEmpty()) return Set.of();

        return usernames.stream()
                .map(String::trim)
                .map(username ->
                        userRepository.findByUsernameAndDeletedFalse(username)
                                .orElseThrow(() ->
                                        new IllegalArgumentException(
                                                "Unknown username: " + username
                                        )
                                ).getId()
                )
                .collect(Collectors.toSet());
    }

    /* ============================================================
       INTERNAL TYPES
    ============================================================ */

    private record PreparedRow(int row, DepartmentDTO dto) {}

    /**
     * Forces rollback but preserves row-level errors
     */
    public static class BulkAbortException extends RuntimeException {
        private final BulkResult<DepartmentDTO> result;

        public BulkAbortException(BulkResult<DepartmentDTO> result) {
            this.result = result;
        }

        public BulkResult<DepartmentDTO> getResult() {
            return result;
        }
    }
}