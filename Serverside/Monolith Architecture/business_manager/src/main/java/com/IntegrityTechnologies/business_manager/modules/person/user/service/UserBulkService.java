package com.IntegrityTechnologies.business_manager.modules.person.user.service;

import com.IntegrityTechnologies.business_manager.config.bulk.BulkOptions;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkRequest;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkResult;
import com.IntegrityTechnologies.business_manager.config.util.PhoneAndEmailNormalizer;
import com.IntegrityTechnologies.business_manager.modules.person.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.department.dto.DepartmentAssignmentDTO;
import com.IntegrityTechnologies.business_manager.modules.person.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.user.dto.UserBulkRow;
import com.IntegrityTechnologies.business_manager.modules.person.user.dto.UserDTO;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.Role;
import com.IntegrityTechnologies.business_manager.modules.person.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.security.util.SecurityUtils;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.interceptor.TransactionAspectSupport;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Service
@RequiredArgsConstructor
@Transactional
public class UserBulkService {

    private final UserService userService;
    private final UserRepository userRepository;
    private final BranchRepository branchRepository;
    private final DepartmentRepository departmentRepository;

    private static final String DEFAULT_PASSWORD = "1234";

    public BulkResult<UserDTO> importUsers(
            BulkRequest<UserBulkRow> request,
            Authentication authentication
    ) {

        BulkResult<UserDTO> result = new BulkResult<>();
        result.setTotal(request.getItems().size());

        BulkOptions options =
                request.getOptions() != null
                        ? request.getOptions()
                        : new BulkOptions();

        Set<String> seenUsernames = new HashSet<>();
        Set<String> seenEmails = new HashSet<>();
        Set<String> seenPhones = new HashSet<>();

        Role currentUserRole = SecurityUtils.currentRole();

        for (int i = 0; i < request.getItems().size(); i++) {
            int rowNum = i + 1;
            UserBulkRow row = request.getItems().get(i);

            try {
                validate(row);

                /* =========================
                   NORMALIZATION
                   ========================= */

                String username = PhoneAndEmailNormalizer.toTitleCase(row.getUsername());
                String roleName =
                        (row.getRole() == null || row.getRole().isBlank())
                                ? Role.EMPLOYEE.name()
                                : row.getRole().trim().toUpperCase();

                List<String> emails = normalizeCommaList(row.getEmailAddresses());
                List<String> phones = normalizeCommaList(row.getPhoneNumbers());

                /* =========================
                   IN-FILE DUPLICATES
                   ========================= */

                if (!seenUsernames.add(username)) {
                    throw new IllegalArgumentException(
                            "Duplicate username in import: " + username
                    );
                }

                for (String e : emails) {
                    if (!seenEmails.add(e)) {
                        throw new IllegalArgumentException(
                                "Duplicate email in import: " + e
                        );
                    }
                }

                for (String p : phones) {
                    if (!seenPhones.add(p)) {
                        throw new IllegalArgumentException(
                                "Duplicate phone in import: " + p
                        );
                    }
                }

                /* =========================
                   DB DUPLICATES
                   ========================= */

                if (options.isSkipDuplicates()
                        && userRepository.existsByUsernameAndTenantId(username, TenantContext.getTenantId())) {
                    throw new IllegalArgumentException(
                            "Username already exists: " + username
                    );
                }

                /* =========================
                   ROLE RESOLUTION
                   ========================= */

                Role targetRole;

                try {
                    targetRole = Role.valueOf(roleName);
                } catch (Exception ex) {
                    throw new IllegalArgumentException(
                            "Invalid role: " + roleName
                    );
                }

                if (!currentUserRole.canAccess(targetRole)) {
                    throw new IllegalArgumentException(
                            "Insufficient privilege to assign role: " + targetRole
                    );
                }

                /* =========================
                   ORG RESOLUTION (OPTIONAL)
                   ========================= */

                List<DepartmentAssignmentDTO> assignments = null;

                if (row.getBranchCode() != null && row.getDepartmentName() != null) {

                    Branch branch = branchRepository.findByTenantIdAndBranchCodeIgnoreCaseAndDeletedFalse(
                        TenantContext.getTenantId(),
                        row.getBranchCode()
                    )
                            .orElseThrow(() ->
                                    new IllegalArgumentException(
                                            "Unknown branchCode: " + row.getBranchCode()
                                    )
                            );

                    Department department = departmentRepository.findByTenantIdAndNameIgnoreCaseAndBranch_Id(
                            TenantContext.getTenantId(),
                            row.getDepartmentName(),
                            branch.getId()
                    ).orElseThrow(() ->
                                    new IllegalArgumentException(
                                            "Unknown department: " + row.getDepartmentName()
                                    )
                            );

                    assignments = List.of(
                            new DepartmentAssignmentDTO(
                                    branch.getId(),
                                    department.getId(),
                                    normalizePosition(row.getPosition())
                            )
                    );
                }

                /* =========================
                   BUILD DTO
                   ========================= */

                UserDTO dto = UserDTO.builder()
                        .username(username)
                        .password(DEFAULT_PASSWORD)
                        .role(targetRole.name())
                        .emailAddresses(emails)
                        .phoneNumbers(phones)
                        .branchCode(
                                row.getBranchCode() != null
                                        ? row.getBranchCode()
                                        : "MAIN"
                        )
                        .departmentName(
                                row.getDepartmentName() != null
                                        ? row.getDepartmentName()
                                        : "GENERAL"
                        )
                        .position(
                                row.getPosition() != null
                                        ? normalizePosition(row.getPosition())
                                        : "member"
                        )
                        .departmentsAndPositions(assignments) // still used on real import
                        .build();

                if (options.isDryRun()) {
                    // 🧪 Dry run → simulate success
                    result.addSuccess(dto);
                } else {
                    // 💾 Actual import
                    UserDTO saved =
                            userService.registerUser(dto, authentication);
                    result.addSuccess(saved);
                }

            } catch (Exception ex) {
                result.addError(rowNum, ex.getMessage());
            }
        }

        // 🔥 HARD GUARANTEE — NO PERSISTENCE ON DRY RUN
        if (options.isDryRun()) {
            TransactionAspectSupport
                    .currentTransactionStatus()
                    .setRollbackOnly();
        }

        return result;
    }

    /* =========================
       HELPERS
       ========================= */

    private void validate(UserBulkRow row) {
        if (row.getUsername() == null || row.getUsername().isBlank())
            throw new IllegalArgumentException("username is required");
    }

    private List<String> normalizeCommaList(List<String> raw) {
        if (raw == null || raw.isEmpty()) return List.of();

        return raw.stream()
                .flatMap(v -> Arrays.stream(v.split(",")))
                .map(String::trim)
                .filter(s -> !s.isBlank())
                .map(String::toLowerCase)
                .distinct()
                .toList();
    }

    private String normalizePosition(String position) {
        if (position == null) return "member";
        String p = position.toLowerCase();
        if (!p.equals("head") && !p.equals("member")) {
            throw new IllegalArgumentException(
                    "position must be 'head' or 'member'"
            );
        }
        return p;
    }
}