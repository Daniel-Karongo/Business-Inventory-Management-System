package com.IntegrityTechnologies.business_manager.modules.person.entity.user.service;

import com.IntegrityTechnologies.business_manager.common.bulk.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.dto.DepartmentAssignmentDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.dto.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import com.IntegrityTechnologies.business_manager.security.acl.entity.RoleEntity;
import com.IntegrityTechnologies.business_manager.security.acl.repository.RoleEntityRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class UserBulkService {

    private final UserService userService;
    private final BranchRepository branchRepository;
    private final DepartmentRepository departmentRepository;
    private final RoleEntityRepository roleEntityRepository;

    private static final String DEFAULT_PASSWORD = "1234";

    public BulkResult<UserDTO> importUsers(
            BulkRequest<UserBulkRow> request,
            Authentication authentication
    ) {

        BulkResult<UserDTO> result = new BulkResult<>();
        result.setTotal(request.getItems().size());

        Role currentUserRole = SecurityUtils.currentRole();

        for (int i = 0; i < request.getItems().size(); i++) {
            int rowNum = i + 1;
            UserBulkRow row = request.getItems().get(i);

            try {
                validate(row);

                /* =========================
                   ROLE RESOLUTION (STRICT)
                   ========================= */

                Role targetRole;
                try {
                    targetRole = Role.valueOf(row.getRole().toUpperCase());
                } catch (Exception ex) {
                    throw new IllegalArgumentException(
                            "Invalid role: " + row.getRole()
                    );
                }

                if (!currentUserRole.canAccess(targetRole)) {
                    throw new IllegalArgumentException(
                            "Insufficient privilege to assign role: " + targetRole
                    );
                }

                RoleEntity roleEntity = roleEntityRepository
                        .findByNameIgnoreCase(targetRole.name())
                        .filter(RoleEntity::isActive)
                        .orElseThrow(() ->
                                new IllegalArgumentException(
                                        "Role is inactive or not configured: " + targetRole
                                )
                        );

                /* =========================
                   ORG RESOLUTION
                   ========================= */

                UUID branchId = branchRepository
                        .findByBranchCode(row.getBranchCode())
                        .orElseThrow(() ->
                                new IllegalArgumentException(
                                        "Unknown branchCode: " + row.getBranchCode()
                                )
                        ).getId();

                Department department = departmentRepository
                        .findByNameIgnoreCase(row.getDepartmentName())
                        .orElseThrow(() ->
                                new IllegalArgumentException(
                                        "Unknown department: " + row.getDepartmentName()
                                )
                        );

                String password =
                        (row.getPassword() != null && !row.getPassword().isBlank())
                                ? row.getPassword()
                                : DEFAULT_PASSWORD;

                UserDTO dto = UserDTO.builder()
                        .username(row.getUsername())
                        .password(password)
                        .role(roleEntity.getName())   // 🔑 canonical role name
                        .emailAddresses(row.getEmailAddresses())
                        .phoneNumbers(row.getPhoneNumbers())
                        .departmentsAndPositions(
                                List.of(
                                        new DepartmentAssignmentDTO(
                                                branchId,
                                                department.getId(),
                                                normalizePosition(row.getPosition())
                                        )
                                )
                        )
                        .build();

                if (!request.getOptions().isDryRun()) {
                    UserDTO saved =
                            userService.registerUser(dto, authentication);
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

    private void validate(UserBulkRow row) {
        if (row.getUsername() == null || row.getUsername().isBlank())
            throw new IllegalArgumentException("username is required");

        if (row.getRole() == null || row.getRole().isBlank())
            throw new IllegalArgumentException("role is required");

        if (row.getBranchCode() == null || row.getBranchCode().isBlank())
            throw new IllegalArgumentException("branchCode is required");

        if (row.getDepartmentName() == null || row.getDepartmentName().isBlank())
            throw new IllegalArgumentException("departmentName is required");
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

    private String generateTemporaryPassword() {
        return UUID.randomUUID()
                .toString()
                .replace("-", "")
                .substring(0, 10);
    }
}