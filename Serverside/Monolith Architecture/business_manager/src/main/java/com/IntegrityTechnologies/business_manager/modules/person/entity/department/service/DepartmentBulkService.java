package com.IntegrityTechnologies.business_manager.modules.person.entity.department.service;

import com.IntegrityTechnologies.business_manager.common.bulk.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.dto.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;

import java.time.LocalTime;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class DepartmentBulkService {

    private final DepartmentService departmentService;
    private final BranchRepository branchRepository;
    private final UserRepository userRepository;

    public BulkResult<DepartmentDTO> importDepartments(
            BulkRequest<DepartmentBulkRow> request,
            Authentication authentication
    ) {

        BulkResult<DepartmentDTO> result = new BulkResult<>();
        result.setTotal(request.getItems().size());

        for (int i = 0; i < request.getItems().size(); i++) {
            int rowNum = i + 1;
            DepartmentBulkRow row = request.getItems().get(i);

            try {
                validate(row);

                Set<UUID> branchIds = resolveBranches(row.getBranchCodes());
                Set<UUID> headIds = resolveUsers(row.getHeadUsernames());
                Set<UUID> memberIds = resolveUsers(row.getMemberUsernames());

                DepartmentDTO dto = new DepartmentDTO();
                dto.setName(row.getName());
                dto.setDescription(row.getDescription());
                dto.setBranchIds(branchIds);
                dto.setHeadIds(headIds);
                dto.setMemberIds(memberIds);

                if (row.getRollcallStartTime() != null) {
                    dto.setRollcallStartTime(
                            LocalTime.parse(row.getRollcallStartTime())
                    );
                }
                dto.setGracePeriodMinutes(row.getGracePeriodMinutes());

                if (!request.getOptions().isDryRun()) {
                    DepartmentDTO saved =
                            departmentService.create(dto, authentication);
                    result.addSuccess(saved);
                }

            } catch (Exception ex) {
                result.addError(rowNum, ex.getMessage());
            }
        }

        return result;
    }

    /* =========================
       VALIDATION
       ========================= */

    private void validate(DepartmentBulkRow row) {
        if (row.getName() == null || row.getName().isBlank()) {
            throw new IllegalArgumentException("name is required");
        }
        if (row.getBranchCodes() == null || row.getBranchCodes().isEmpty()) {
            throw new IllegalArgumentException("At least one branchCode is required");
        }
    }

    /* =========================
       RESOLVERS
       ========================= */

    private Set<UUID> resolveBranches(Set<String> branchCodes) {
        return branchCodes.stream()
                .map(code ->
                        branchRepository.findByBranchCode(code)
                                .orElseThrow(() ->
                                        new IllegalArgumentException(
                                                "Unknown branchCode: " + code
                                        )
                                ).getId()
                )
                .collect(Collectors.toSet());
    }

    private Set<UUID> resolveUsers(Set<String> usernames) {
        if (usernames == null || usernames.isEmpty()) return Set.of();

        return usernames.stream()
                .map(username ->
                        userRepository.findByUsername(username)
                                .orElseThrow(() ->
                                        new IllegalArgumentException(
                                                "Unknown username: " + username
                                        )
                                ).getId()
                )
                .collect(Collectors.toSet());
    }
}