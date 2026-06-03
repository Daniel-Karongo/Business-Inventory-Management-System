package com.IntegrityTechnologies.business_manager.modules.person.user.service;

import com.IntegrityTechnologies.business_manager.config.bulk.BulkOptions;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkRequest;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkResult;
import com.IntegrityTechnologies.business_manager.config.util.PhoneAndEmailNormalizer;
import com.IntegrityTechnologies.business_manager.modules.person.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.user.dto.UserBulkRow;
import com.IntegrityTechnologies.business_manager.modules.person.user.dto.UserDTO;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.Role;
import com.IntegrityTechnologies.business_manager.modules.person.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.security.cache.TenantMetadataCache;
import com.IntegrityTechnologies.business_manager.security.util.SecurityUtils;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.io.IOException;
import java.util.*;

@Service
@RequiredArgsConstructor
public class UserBulkService {

    private final UserService userService;
    private final UserRepository userRepository;
    private final BranchRepository branchRepository;

    private static final String DEFAULT_PASSWORD = "1234";
    private final TenantMetadataCache tenantMetadataCache;

    @Transactional
    public BulkResult<UserDTO> importUsers(
            BulkRequest<UserBulkRow> request,
            Authentication authentication
    ) throws IOException {

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

        /*
         * PREPARED DTOS
         * (validated + normalized, not yet persisted)
         */
        List<UserDTO> prepared = new ArrayList<>();

        UUID tenantId = TenantContext.getTenantId();

        int existingUsers =
                userRepository.countByTenantId(tenantId);

        int incomingUsers =
                request.getItems().size();

        int maxUsers =
                tenantMetadataCache
                        .getSubscriptionPlan(tenantId)
                        .getMaxUsers();

        if ((existingUsers + incomingUsers) > maxUsers) {

            throw new IllegalArgumentException(
                    "Import exceeds subscription user limit. "
                            + "Current users: " + existingUsers
                            + ", incoming users: " + incomingUsers
                            + ", max allowed: " + maxUsers
            );
        }
        
    /* =====================================================
       PASS 1 — VALIDATION ONLY (NO DB WRITES)
    ===================================================== */

        for (int i = 0; i < request.getItems().size(); i++) {

            int rowNum = i + 1;

            UserBulkRow row = request.getItems().get(i);

            try {

                validate(row);

            /* =========================
               NORMALIZATION
            ========================= */

                String username =
                        PhoneAndEmailNormalizer.toTitleCase(
                                row.getUsername()
                        );

                String roleName =
                        (row.getRole() == null || row.getRole().isBlank())
                                ? Role.EMPLOYEE.name()
                                : row.getRole().trim().toUpperCase();

                List<String> emails =
                        normalizeCommaList(row.getEmailAddresses());

                List<String> phones =
                        normalizeCommaList(row.getPhoneNumbers());

            /* =========================
               IN-FILE DUPLICATES
            ========================= */

                if (!seenUsernames.add(username)) {
                    throw new IllegalArgumentException(
                            "Duplicate username in file: " + username
                    );
                }

                for (String e : emails) {
                    if (!seenEmails.add(e)) {
                        throw new IllegalArgumentException(
                                "Duplicate email in file: " + e
                        );
                    }
                }

                for (String p : phones) {
                    if (!seenPhones.add(p)) {
                        throw new IllegalArgumentException(
                                "Duplicate phone in file: " + p
                        );
                    }
                }

            /* =========================
               DB DUPLICATES
            ========================= */

                if (
                        options.isSkipDuplicates()
                                && userRepository.existsByUsernameAndTenantId(
                                username,
                                TenantContext.getTenantId()
                        )
                ) {
                    throw new IllegalArgumentException(
                            "Username already exists: " + username
                    );
                }

                for (String email : emails) {

                    if (
                            userRepository.existsByEmailAddressAndTenantId(
                                    email,
                                    TenantContext.getTenantId()
                            )
                    ) {
                        throw new IllegalArgumentException(
                                "Email already exists: " + email
                        );
                    }
                }

                for (String phone : phones) {

                    if (
                            userRepository.existsByPhoneNumberAndTenantId(
                                    phone,
                                    TenantContext.getTenantId()
                            )
                    ) {
                        throw new IllegalArgumentException(
                                "Phone number already exists: " + phone
                        );
                    }
                }

            /* =========================
               ROLE RESOLUTION
            ========================= */

                Role targetRole;

                try {

                    targetRole =
                            Role.valueOf(roleName);

                } catch (Exception ex) {

                    throw new IllegalArgumentException(
                            "Invalid role: " + roleName
                    );
                }

                if (!currentUserRole.canManage(targetRole)) {

                    throw new IllegalArgumentException(
                            "Insufficient privilege to assign role: "
                                    + targetRole
                    );
                }

            /* =========================
               ORG VALIDATION
               (NO PERSISTENCE)
            ========================= */

                String branchCode =
                        row.getBranchCode() != null
                                ? row.getBranchCode().trim()
                                : "MAIN";

                Branch branch =
                        branchRepository
                                .findByTenantIdAndBranchCodeIgnoreCaseAndDeletedFalse(
                                        TenantContext.getTenantId(),
                                        branchCode
                                )
                                .orElseThrow(() ->
                                        new IllegalArgumentException(
                                                "Unknown branchCode: " + branchCode
                                        )
                                );

            /* =========================
               BUILD DTO
            ========================= */

                UserDTO dto =
                        UserDTO.builder()
                                .username(username)
                                .password(
                                        row.getPassword() != null
                                                ? String.valueOf(row.getPassword())
                                                : DEFAULT_PASSWORD
                                )
                                .role(targetRole.name())
                                .emailAddresses(emails)
                                .phoneNumbers(phones)
                                .branchCode(branchCode)
                                .branchIds(List.of(branch.getId()))
                                .build();

                prepared.add(dto);

                /*
                 * PREVIEW SUCCESS
                 * (validation success only)
                 */
                result.addSuccess(dto);

            } catch (Exception ex) {

                result.addError(
                        rowNum,
                        ex.getMessage()
                );
            }
        }

    /* =====================================================
       ❌ VALIDATION FAILED → ABORT BEFORE PERSISTENCE
    ===================================================== */

        if (!result.getErrors().isEmpty()) {

            /*
             * Preview should not appear as persisted success
             */
            result.setSuccess(0);

            return result;
        }

    /* =====================================================
       🧪 DRY RUN → RETURN PREVIEW ONLY
    ===================================================== */

        if (options.isDryRun()) {
            return result;
        }

    /* =====================================================
       PASS 2 — PERSIST (ALL OR NOTHING)
       NO TRY/CATCH
    ===================================================== */

        result.getData().clear();

        result.setSuccess(0);

        for (UserDTO dto : prepared) {

            UserDTO saved =
                    userService.registerUser(
                            dto,
                            authentication
                    );

            result.addSuccess(saved);
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
}