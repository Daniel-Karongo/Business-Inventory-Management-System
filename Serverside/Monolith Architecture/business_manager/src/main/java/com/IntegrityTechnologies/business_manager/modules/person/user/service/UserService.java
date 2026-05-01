package com.IntegrityTechnologies.business_manager.modules.person.user.service;

import com.IntegrityTechnologies.business_manager.config.caffeine.CacheInvalidationService;
import com.IntegrityTechnologies.business_manager.config.files.FIleUploadDTO;
import com.IntegrityTechnologies.business_manager.config.files.FileStorageService;
import com.IntegrityTechnologies.business_manager.config.files.TransactionalFileManager;
import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.config.util.PhoneAndEmailNormalizer;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.exception.InvalidUserDataException;
import com.IntegrityTechnologies.business_manager.exception.UnauthorizedAccessException;
import com.IntegrityTechnologies.business_manager.exception.UserNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.person.branch.dto.BranchHierarchyDTO;
import com.IntegrityTechnologies.business_manager.modules.person.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.department.dto.DepartmentAssignmentDTO;
import com.IntegrityTechnologies.business_manager.modules.person.department.dto.DepartmentPositionDTO;
import com.IntegrityTechnologies.business_manager.modules.person.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.department.model.DepartmentMembershipRole;
import com.IntegrityTechnologies.business_manager.modules.person.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.user.dto.UserDTO;
import com.IntegrityTechnologies.business_manager.modules.person.user.mapper.UserImageMapper;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.*;
import com.IntegrityTechnologies.business_manager.modules.person.user.repository.*;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.service.SubscriptionGuard;
import com.IntegrityTechnologies.business_manager.security.auth.service.AuthService;
import com.IntegrityTechnologies.business_manager.security.util.PrivilegesChecker;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.Hibernate;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.util.*;

@Service
@RequiredArgsConstructor
@Slf4j
public class UserService {

    private final UserRepository userRepository;
    private final UserImageRepository userImageRepository;
    private final UserAuditRepository userAuditRepository;
    private final UserImageAuditRepository userImageAuditRepository;
    private final PasswordEncoder passwordEncoder;
    private final UserImageService userImageService;
    private final PrivilegesChecker privilegesChecker;
    private final FileStorageService fileStorageService;
    private final TransactionalFileManager transactionalFileManager;
    private final DepartmentRepository departmentRepository;
    private final BranchRepository branchRepository;
    private final UserDepartmentRepository userDepartmentRepository;
    private final UserBranchRepository userBranchRepository;
    private final SubscriptionGuard subscriptionGuard;
    private final AuthService authService;
    private final CacheInvalidationService cacheInvalidationService;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    /* ====================== REGISTER USER ====================== */
    @Transactional
    public UserDTO registerUser(UserDTO dto, Authentication authentication) throws IOException {
        subscriptionGuard.checkUserLimit();

        String creatorUsername = (authentication != null && authentication.getPrincipal() instanceof UserDetails userDetails)
                ? userDetails.getUsername()
                : null;

        // 0️⃣ Normalize input
        Set<String> emails = PhoneAndEmailNormalizer.normalizeEmails(dto.getEmailAddresses());
        Set<String> phones = PhoneAndEmailNormalizer.normalizePhones(dto.getPhoneNumbers());

//        if (emails.isEmpty()) throw new InvalidUserDataException("At least one email address is required");

        // 1️⃣ Validate uniqueness
        checkEmailUniqueness(emails);
        checkPhoneUniqueness(phones);

        if (userRepository.existsByUsernameAndTenantId(
                dto.getUsername(),
                TenantContext.getTenantId()
        ))
            throw new InvalidUserDataException("Username already in use");

        if (dto.getIdNumber() != null &&
                userRepository.existsByIdNumberAndTenantId(
                        dto.getIdNumber(),
                        TenantContext.getTenantId()
                ))
            throw new InvalidUserDataException("ID number already in use");

        // 2️⃣ Resolve creator

        User creator = creatorUsername != null
                ? userRepository.findByUsernameAndTenantId(creatorUsername, tenantId()).orElse(null)
                : null;
        Role newUserRole = Role.valueOf(dto.getRole().trim().toUpperCase());

        if (creator != null) {
            Role creatorRole = creator.getRole();

            if (!creatorRole.canManage(newUserRole)) {
                throw new UnauthorizedAccessException(
                        "You cannot create a user with role "
                                + newUserRole
                                + " because your role is "
                                + creatorRole
                );
            }
        }

        // 3️⃣ Generate upload folder upfront
        String uploadFolder = UUID.randomUUID() + "_" + System.currentTimeMillis();

        // 4️⃣ Build & save user
        User user = User.builder()
                .username(dto.getUsername())
                .password(dto.getPassword() != null
                        ? passwordEncoder.encode(dto.getPassword())
                        : passwordEncoder.encode(""))
                .emailAddresses(new ArrayList<>(emails))
                .phoneNumbers(new ArrayList<>(phones))
                .idNumber(dto.getIdNumber())
                .role(Role.valueOf(dto.getRole().trim().toUpperCase()))
                .deleted(false)
                .uploadFolder(uploadFolder)
                .build();

        try {
            user = userRepository.save(user);
        } catch (DataIntegrityViolationException ex) {

            if (ex.getMessage().contains("uq_user_email_per_tenant")) {
                throw new InvalidUserDataException("Email already exists");
            }

            if (ex.getMessage().contains("uq_user_phone_per_tenant")) {
                throw new InvalidUserDataException("Phone number already exists");
            }

            throw ex;
        }

        boolean assignedDefaults = applyDefaultBranchAndDepartmentIfMissing(dto, user);

        if (!assignedDefaults) {
            // Only apply admin assignments when defaults were NOT used
            applyDepartmentAndBranchAssignments(dto, user, authentication);
        }

        // 5️⃣ Create actual folder on disk
        Path userFolderPath = fileStorageService.initDirectory(
                fileStorageService.userRoot().resolve(uploadFolder)
        );

        transactionalFileManager.track(userFolderPath);


        // 6️⃣ Upload images
        uploadUserImages(dto.getUserFiles(), uploadFolder, user,
                creator != null ? creator.getId() : null,
                creatorUsername);

        // 7️⃣ Audit creation
        userAuditRepository.save(UserAudit.builder()
                .userId(user.getId())
                .username(user.getUsername())
                .action("CREATE")
                .performedById(creator != null ? creator.getId() : null)
                .performedByUsername(creatorUsername)
                .timestamp(LocalDateTime.now())
                .build());

        Hibernate.initialize(user.getEmailAddresses());
        Hibernate.initialize(user.getPhoneNumbers());

        evictUsersAfterCommit();

        return mapToDTO(user);
    }


    /* ====================== UPDATE USER ====================== */
    @Transactional
    public UserDTO updateUser(String identifier, UserDTO updatedData, Authentication authentication) throws IOException {

        boolean usernameChanged = false;
        boolean passwordChanged = false;

        String updaterUsername = (authentication != null && authentication.getPrincipal() instanceof UserDetails userDetails)
                ? userDetails.getUsername()
                : null;

        // 1️⃣ Lookup target user
        User user = userRepository.findByIdentifier(identifier, tenantId())
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        // 2️⃣ Lookup updater
        User updater = userRepository.findByIdentifier(updaterUsername, tenantId())
                .orElseThrow(() -> new UserNotFoundException("Updater not found"));

        // 3️⃣ Validate access using centralized method
        if (!privilegesChecker.isAuthorizedWithinDepartment(updater, user)) {
            throw new UnauthorizedAccessException(
                    "You are not authorized to update user: " + user.getUsername()
            );
        }

        Map<String, String[]> changes = new HashMap<>();

        // 4️⃣ Username
        if (updatedData.getUsername() != null
                && !updatedData.getUsername().isBlank()
                && !updatedData.getUsername().equals(user.getUsername())) {

            if (userRepository.existsByUsernameAndTenantId(
                    updatedData.getUsername(),
                    tenantId()
            )) {
                throw new InvalidUserDataException("Username already in use");
            }

            changes.put(
                    "username",
                    new String[]{user.getUsername(), updatedData.getUsername()}
            );

            user.setUsername(updatedData.getUsername());

            usernameChanged = true;
        }

        // 5️⃣ Emails
        if (updatedData.getEmailAddresses() != null) {
            List<String> oldEmails = user.getEmailAddresses() != null ? user.getEmailAddresses() : List.of();
            Set<String> newEmailsSet = PhoneAndEmailNormalizer.normalizeEmails(updatedData.getEmailAddresses());
            List<String> newEmails = new ArrayList<>(newEmailsSet);
            checkEmailUniqueness(newEmails, user.getId(), oldEmails);
            updateEmailsAndAudit(user, oldEmails, newEmails, updaterUsername);
        }

        // 6️⃣ Phones
        if (updatedData.getPhoneNumbers() != null) {
            List<String> oldPhones = user.getPhoneNumbers() != null ? user.getPhoneNumbers() : List.of();
            Set<String> newPhonesSet = PhoneAndEmailNormalizer.normalizePhones(updatedData.getPhoneNumbers());
            List<String> newPhones = new ArrayList<>(newPhonesSet);
            checkPhoneUniqueness(newPhones, user.getId(), oldPhones);
            updatePhonesAndAudit(user, oldPhones, newPhones, updaterUsername);
        }

        // 7️⃣ ID Number
        if (updatedData.getIdNumber() != null && !updatedData.getIdNumber().isBlank()
                && !updatedData.getIdNumber().equals(user.getIdNumber())) {
            if (userRepository.existsByIdNumberAndTenantId(
                    updatedData.getIdNumber(),
                    TenantContext.getTenantId()
            ))
                throw new InvalidUserDataException("ID number already in use");
            changes.put("idNumber", new String[]{user.getIdNumber(), updatedData.getIdNumber()});
            user.setIdNumber(updatedData.getIdNumber());
        }

        // 8️⃣ Password (SELF-SERVICE ONLY ENFORCEMENT)
        if (updatedData.getPassword() != null
                && !updatedData.getPassword().isBlank()) {
        /*
         Only the authenticated user may change
         their own password.
        */
            if (!updater.getId().equals(user.getId())) {

                userAuditRepository.save(
                        UserAudit.builder()
                                .userId(user.getId())
                                .username(user.getUsername())
                                .action("PASSWORD_CHANGE_BLOCKED")
                                .reason("Attempted password change for another user denied")
                                .performedById(updater.getId())
                                .performedByUsername(updaterUsername)
                                .timestamp(LocalDateTime.now())
                                .build()
                );

                throw new UnauthorizedAccessException(
                        "Users may only change their own password."
                );
            }

            user.setPassword(
                    passwordEncoder.encode(
                            updatedData.getPassword()
                    )
            );

            passwordChanged = true;
        }

        // 9️⃣ Role
        if (updatedData.getRole() != null) {

            if (user.getId().equals(updater.getId())) {
                throw new UnauthorizedAccessException(
                        "You cannot change your own role"
                );
            }

            if (!updater.getRole().canManage(user.getRole())) {
                throw new UnauthorizedAccessException(
                        "You cannot modify a peer or higher role"
                );
            }

            Role newRole =
                    Role.valueOf(updatedData.getRole().toUpperCase());

            if (!updater.getRole().canManage(newRole)) {
                throw new UnauthorizedAccessException(
                        "You are not authorized to assign this role"
                );
            }

            changes.put(
                    "role",
                    new String[]{
                            user.getRole().name(),
                            newRole.name()
                    }
            );

            user.setRole(newRole);
        }

        // 1️⃣0️⃣ Save and audit
        user = userRepository.save(user);

        boolean credentialsChanged =
                usernameChanged || passwordChanged;

        if (credentialsChanged) {

            authService.logoutAllSessions(
                    user.getId(),
                    true
            );

            userAuditRepository.save(
                    UserAudit.builder()
                            .userId(user.getId())
                            .username(user.getUsername())
                            .action("FORCED_LOGOUT_ALL")
                            .reason("All sessions revoked due to credential change")
                            .performedById(updater.getId())
                            .performedByUsername(updaterUsername)
                            .timestamp(LocalDateTime.now())
                            .build()
            );
        }

        applyDepartmentAndBranchAssignments(updatedData, user, authentication);

        auditChanges(user, changes, updaterUsername);

        Hibernate.initialize(user.getEmailAddresses());
        Hibernate.initialize(user.getPhoneNumbers());

        evictUsersAfterCommit();

        return mapToDTO(user);
    }

    @Transactional
    public ResponseEntity<?> updateUserImages(
            String identifier,
            List<FIleUploadDTO> newFiles,
            Authentication auth,
            Boolean deleteOldImages
    ) throws IOException {

        User target = validateAccess(identifier, auth, "update images");

        Path userDir = fileStorageService.userRoot()
                .resolve(target.getUploadFolder())
                .normalize();

    /* =====================================================
       1) Optionally soft-delete existing images
       ===================================================== */
        if (Boolean.TRUE.equals(deleteOldImages)) {
            softDeleteAllUserImages(target, userDir, auth);
        }

    /* =====================================================
       2) Save new uploaded files
       ===================================================== */
        if (newFiles != null && !newFiles.isEmpty()) {
            saveNewUserImages(target, newFiles, auth);
        }

        userRepository.save(target);
        Hibernate.initialize(target.getEmailAddresses());
        Hibernate.initialize(target.getPhoneNumbers());

        cacheInvalidationService.evictUsers(tenantId());

        return ResponseEntity.ok(mapToDTO(target));
    }

    /* ====================== IMAGE & USER ACCESS VALIDATION ====================== */
    private User validateAccess(String identifier, Authentication auth, String action) {
        User target = userRepository.findByIdentifier(identifier, tenantId())
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        User requester = privilegesChecker.getAuthenticatedUser(auth);
        if (!privilegesChecker.isAuthorizedWithinDepartment(requester, target)) {
            throw new UnauthorizedAccessException("You are not authorized to " + action + " for user: " + target.getUsername());
        }
        return target;
    }

    private boolean hasManagerialRole(String username) {
        return userRepository
                .findByUsernameAndTenantId(username, tenantId())
                .map(user ->
                        user.getRole() == Role.SUPERUSER ||
                                user.getRole() == Role.ADMIN ||
                                user.getRole() == Role.MANAGER
                )
                .orElse(false);
    }

    /* ====================== GET USERS ====================== */

    @Cacheable(
            value = "user-by-identifier",
            key = "T(com.IntegrityTechnologies.business_manager.config.caffeine.CacheKeys)" +
                    ".user(" +
                    "T(com.IntegrityTechnologies.business_manager.security.util.TenantContext).getTenantId(), " +
                    "#identifier, #deleted)"
    )
    @Transactional(readOnly = true)
    public UserDTO getUser(String identifier, Boolean deleted) {

        User user;

        if (Boolean.TRUE.equals(deleted)) {

            user = userRepository
                    .findDeletedByIdentifier(identifier, tenantId())
                    .orElseThrow(() -> new UserNotFoundException("User not found: " + identifier));

        } else if (Boolean.FALSE.equals(deleted)) {

            user = userRepository
                    .findByIdentifier(identifier, tenantId())
                    .orElseThrow(() -> new UserNotFoundException("User not found: " + identifier));

        } else {

            user = userRepository
                    .findByIdentifierIncludingDeleted(identifier, tenantId())
                    .orElseThrow(() -> new UserNotFoundException("User not found: " + identifier));

        }

        Hibernate.initialize(user.getEmailAddresses());
        Hibernate.initialize(user.getPhoneNumbers());
        return mapToDTO(user);
    }

    @Cacheable(
            value = "users-page",
            key = "T(com.IntegrityTechnologies.business_manager.config.caffeine.CacheKeys)" +
                    ".usersPage(" +
                    "T(com.IntegrityTechnologies.business_manager.security.util.TenantContext).getTenantId(), " +
                    "#deleted, #role, #branchId, #departmentId, #q, " +
                    "#pageable.pageNumber, #pageable.pageSize, #pageable.sort.toString())"
    )
    @Transactional(readOnly = true)
    public Page<UserDTO> getUsersFiltered(
            Boolean deleted,
            String role,
            UUID branchId,
            UUID departmentId,
            String q,
            Pageable pageable
    ) {

        Role parsedRole = role != null ? Role.valueOf(role.toUpperCase()) : null;

        // 🔥 REMOVE SORTING FROM DB QUERY (fix MySQL DISTINCT issue)
        Pageable unsortedPageable = Pageable.ofSize(pageable.getPageSize())
                .withPage(pageable.getPageNumber());

        // STEP 1: fetch IDs only (safe pagination)
        Page<UUID> pageIds = userRepository.searchUserIds(
                tenantId(),
                deleted,
                parsedRole,
                branchId,
                departmentId,
                q,
                unsortedPageable
        );

        if (pageIds.isEmpty()) {
            return new PageImpl<>(List.of(), pageable, 0);
        }

        // STEP 2: fetch full users
        List<User> users = userRepository.findUsersWithDepartments(pageIds.getContent());

        // 🔥 STEP 3: preserve DB pagination order
        Map<UUID, Integer> orderMap = new HashMap<>();
        for (int i = 0; i < pageIds.getContent().size(); i++) {
            orderMap.put(pageIds.getContent().get(i), i);
        }

        users.sort(Comparator.comparingInt(u -> orderMap.get(u.getId())));

        // 🔥 STEP 4: APPLY SORTING IN MEMORY (SAFE)
        if (pageable.getSort().isSorted()) {
            Comparator<User> comparator = null;

            for (Sort.Order order : pageable.getSort()) {

                Comparator<User> fieldComparator = getComparatorForField(order.getProperty());

                if (fieldComparator == null) continue;

                if (order.isDescending()) {
                    fieldComparator = fieldComparator.reversed();
                }

                comparator = (comparator == null)
                        ? fieldComparator
                        : comparator.thenComparing(fieldComparator);
            }

            if (comparator != null) {
                users.sort(comparator);
            }
        }

        // STEP 5: map to DTO
        List<UserDTO> dtoList = users.stream().map(user -> {
            Hibernate.initialize(user.getEmailAddresses());
            Hibernate.initialize(user.getPhoneNumbers());
            return mapToDTO(user);
        }).toList();

        return new PageImpl<>(dtoList, pageable, pageIds.getTotalElements());
    }

    private Comparator<User> getComparatorForField(String field) {

        return switch (field) {

            case "username" -> Comparator.comparing(u -> Optional.ofNullable(u.getUsername()).orElse("").toLowerCase());

            case "role" -> Comparator.comparing(u -> Optional.ofNullable(u.getRole()).map(Enum::name).orElse(""));

            case "createdAt" ->
                    Comparator.comparing(User::getCreatedAt, Comparator.nullsLast(Comparator.naturalOrder()));

            case "deleted" -> Comparator.comparing(u -> Optional.ofNullable(u.getDeleted()).orElse(false));

            default -> null;
        };
    }

    @Transactional(readOnly = true)
    public Page<UserDTO> getUsersByRole(
            Role requestedRole,
            Boolean deleted,
            Pageable pageable
    ) {

        Role currentUserRole = privilegesChecker.getCurrentUserRole();

        if (!currentUserRole.canAccess(requestedRole)) {
            throw new UnauthorizedAccessException("You cannot access users of this role.");
        }

        Page<User> users;

        if (Boolean.TRUE.equals(deleted)) {

            users = userRepository.findDeletedUsersByRole(
                    tenantId(),
                    requestedRole,
                    pageable
            );

        } else if (Boolean.FALSE.equals(deleted)) {

            users = userRepository.findActiveUsersByRole(
                    tenantId(),
                    requestedRole,
                    pageable
            );

        } else {

            users = userRepository.findUsersByRole(
                    tenantId(),
                    requestedRole,
                    pageable
            );
        }

        return users.map(user -> {
            Hibernate.initialize(user.getEmailAddresses());
            Hibernate.initialize(user.getPhoneNumbers());
            return mapToDTO(user);
        });
    }

    public User getUserByIdentifierForAudits(String identifier) {
        return userRepository.findByIdentifierForAudits(identifier, tenantId())
                .orElseThrow(() -> new UserNotFoundException("User not found"));
    }

    public Page<UserAudit> getUserAuditsTarget(UUID userId, Pageable pageable) {
        return userAuditRepository
                .findByUserIdOrderByTimestampDesc(tenantId(), userId, pageable);
    }

    public List<UserAudit> getUserAuditsPerpetrated(UUID userId) {
        return userAuditRepository.findByPerformedByIdOrderByTimestampDesc(tenantId(), userId);
    }

    public List<UserImageAudit> getUserImageAuditsTarget(UUID userId) {
        return userImageAuditRepository.findByUserIdOrderByTimestampDesc(tenantId(), userId);
    }

    public List<UserImageAudit> getUserImageAuditsPerpetrated(UUID userId) {
        return userImageAuditRepository.findByPerformedByIdOrderByTimestampDesc(tenantId(), userId);
    }


    /* ====================== SOFT DELETE / RESTORE ====================== */
    // ====================== SOFT DELETE ======================

    @Transactional
    public ResponseEntity<ApiResponse> softDeleteUser(UUID id, Authentication authentication, String reason) {
        User user = userRepository
                .findByIdAndTenantId(id, tenantId())
                .orElseThrow(() -> new UserNotFoundException("User not found: " + id));

        User actor = privilegesChecker.getAuthenticatedUser(authentication);

        if (!privilegesChecker.isAuthorizedWithinDepartment(actor, user)) {
            throw new UnauthorizedAccessException("Not authorized to modify this user");
        }

        if (actor.getId().equals(user.getId())) {
            throw new UnauthorizedAccessException("Self destructive operations prohibited");
        }

        UUID performedById = actor.getId();
        String performedByUsername = actor.getUsername();

        Map<String, Object> modified = softDeleteUserInternal(
                user,
                performedById,
                performedByUsername,
                reason
        );

        evictUsersAfterCommit();

        return ResponseEntity.ok(new ApiResponse(
                "success",
                "User soft deleted successfully",
                List.of(modified)
        ));
    }

    @Transactional
    public ResponseEntity<ApiResponse> softDeleteUsersInBulk(
            List<UUID> userIds,
            String reason,
            Authentication authentication
    ) {

        User actor =
                privilegesChecker.getAuthenticatedUser(authentication);

        UUID performedById = actor.getId();
        String performedByUsername = actor.getUsername();

        List<Map<String, Object>> modifiedUsers =
                new ArrayList<>();

        for (UUID id : userIds) {

            User user = userRepository
                    .findByIdAndTenantId(id, tenantId())
                    .orElseThrow(() ->
                            new UserNotFoundException(
                                    "User not found: " + id
                            )
                    );

            validateManagementAction(
                    actor,
                    user,
                    false
            );

            modifiedUsers.add(
                    softDeleteUserInternal(
                            user,
                            performedById,
                            performedByUsername,
                            reason
                    )
            );
        }

        evictUsersAfterCommit();
        return ResponseEntity.ok(
                new ApiResponse(
                        "success",
                        "Users soft deleted successfully",
                        modifiedUsers
                )
        );
    }

    /**
     * Internal helper for soft delete
     */
    private Map<String, Object> softDeleteUserInternal(User user, UUID performedById, String performedByUsername, String reason) {
        // Audit user
        userAuditRepository.save(UserAudit.builder()
                .userId(user.getId())
                .username(user.getUsername())
                .role(user.getRole() != null ? user.getRole().name() : null)
                .action("SOFT_DELETE")
                .reason(reason == null || reason.isBlank() ? "Administration decision" : reason)
                .performedById(performedById)
                .performedByUsername(performedByUsername)
                .timestamp(LocalDateTime.now())
                .build());

        // Soft delete images and audit
        if (user.getImages() != null) {
            user.getImages().forEach(image -> {
                image.setDeleted(true);
                userImageAuditRepository.save(UserImageAudit.builder()
                        .userId(user.getId())
                        .username(user.getUsername())
                        .fileName(image.getFileName())
                        .filePath(image.getFilePath())
                        .action("SOFT_DELETE")
                        .reason("Soft deletion due to user soft delete")
                        .performedById(performedById)
                        .performedByUsername(performedByUsername)
                        .timestamp(LocalDateTime.now())
                        .build());
            });
        }

        // Mark deleted
        user.setDeleted(true);
        userRepository.save(user);

        return Map.of("username", user.getUsername());
    }

// ====================== RESTORE ======================

    @Transactional
    public ResponseEntity<ApiResponse> restoreUser(UUID id, Authentication authentication, String reason) throws IOException {

        User user = userRepository
                .findByIdAndTenantId(id, tenantId())
                .orElseThrow(() -> new UserNotFoundException("User not found: " + id));

        User actor =
                privilegesChecker.getAuthenticatedUser(authentication);

        if (!privilegesChecker.isAuthorizedWithinDepartment(actor, user)) {
            throw new UnauthorizedAccessException(
                    "Not authorized to modify this user"
            );
        }

        if (actor.getId().equals(user.getId())) {
            throw new UnauthorizedAccessException(
                    "You cannot restore yourself"
            );
        }

        UUID performedById = privilegesChecker.getAuthenticatedUser(authentication).getId();
        String performedByUsername = privilegesChecker.getAuthenticatedUser(authentication).getUsername();

        Map<String, Object> restored = restoreUserInternal(user, performedById, performedByUsername, reason);

        evictUsersAfterCommit();

        return ResponseEntity.ok(new ApiResponse(
                "success",
                "User restored successfully",
                List.of(restored)
        ));
    }

    @Transactional
    public ResponseEntity<ApiResponse> restoreUsersInBulk(
            List<UUID> userIds,
            String reason,
            Authentication authentication
    ) throws IOException {

        User actor =
                privilegesChecker.getAuthenticatedUser(authentication);

        UUID performedById = actor.getId();
        String performedByUsername = actor.getUsername();

        List<Map<String, Object>> restoredUsers =
                new ArrayList<>();

        for (UUID id : userIds) {

            User user = userRepository
                    .findByIdAndTenantId(id, tenantId())
                    .orElseThrow(() ->
                            new UserNotFoundException(
                                    "User not found: " + id
                            )
                    );

            validateManagementAction(
                    actor,
                    user,
                    false
            );

            restoredUsers.add(
                    restoreUserInternal(
                            user,
                            performedById,
                            performedByUsername,
                            reason
                    )
            );
        }

        evictUsersAfterCommit();

        return ResponseEntity.ok(
                new ApiResponse(
                        "success",
                        "Users restored successfully",
                        restoredUsers
                )
        );
    }

    /**
     * Internal helper for restore
     */
    private Map<String, Object> restoreUserInternal(User user, UUID performedById, String performedByUsername, String reason) throws IOException {
        // Audit restore
        userAuditRepository.save(UserAudit.builder()
                .userId(user.getId())
                .username(user.getUsername())
                .role(user.getRole() != null ? user.getRole().name() : null)
                .action("RESTORE")
                .reason(reason == null ? "Administration decision" : reason)
                .performedById(performedById)
                .performedByUsername(performedByUsername)
                .timestamp(LocalDateTime.now())
                .build());

        // Restore user
        user.setDeleted(false);
        userRepository.save(user);

        // Restore images in DB & audit
        if (user.getImages() != null) {
            user.getImages().forEach(image -> {
                image.setDeleted(false);
                userImageAuditRepository.save(UserImageAudit.builder()
                        .userId(user.getId())
                        .username(user.getUsername())
                        .fileName(image.getFileName())
                        .filePath(image.getFilePath())
                        .action("RESTORE")
                        .reason("Restoring image along with user")
                        .performedById(performedById)
                        .performedByUsername(performedByUsername)
                        .timestamp(LocalDateTime.now())
                        .build());
            });
        }

        return Map.of("username", user.getUsername());
    }

// ====================== HARD DELETE ======================

    @Transactional
    public ResponseEntity<ApiResponse> hardDeleteUser(UUID id, Authentication authentication) throws IOException {
        User user = userRepository
                .findByIdAndTenantId(id, tenantId())
                .orElseThrow(() -> new UserNotFoundException("User not found: " + id));

        User actor =
                privilegesChecker.getAuthenticatedUser(authentication);

        if (!privilegesChecker.isAuthorizedWithinDepartment(actor, user)) {
            throw new UnauthorizedAccessException(
                    "Not authorized to modify this user"
            );
        }

        if (actor.getId().equals(user.getId())) {
            throw new UnauthorizedAccessException(
                    "Self destructive operations prohibited"
            );
        }

        UUID performedById = privilegesChecker.getAuthenticatedUser(authentication).getId();
        String performedByUsername = privilegesChecker.getAuthenticatedUser(authentication).getUsername();

        Map<String, Object> deleted = hardDeleteUserInternal(user, performedById, performedByUsername);

        evictUsersAfterCommit();

        return ResponseEntity.ok(new ApiResponse(
                "success",
                "User permanently deleted",
                List.of(deleted)
        ));
    }

    @Transactional
    public ResponseEntity<ApiResponse> hardDeleteUsersInBulk(
            List<UUID> userIds,
            Authentication authentication
    ) throws IOException {

        User actor =
                privilegesChecker.getAuthenticatedUser(authentication);

        UUID performedById = actor.getId();
        String performedByUsername = actor.getUsername();

        List<Map<String, Object>> deletedUsers =
                new ArrayList<>();

        for (UUID id : userIds) {

            User user = userRepository
                    .findByIdAndTenantId(id, tenantId())
                    .orElseThrow(() ->
                            new UserNotFoundException(
                                    "User not found: " + id
                            )
                    );

            validateManagementAction(
                    actor,
                    user,
                    false
            );

            deletedUsers.add(
                    hardDeleteUserInternal(
                            user,
                            performedById,
                            performedByUsername
                    )
            );
        }

        evictUsersAfterCommit();

        return ResponseEntity.ok(
                new ApiResponse(
                        "success",
                        "Users permanently deleted",
                        deletedUsers
                )
        );
    }

    /**
     * Internal helper for hard delete
     */
    private Map<String, Object> hardDeleteUserInternal(User user, UUID performedById, String performedByUsername) throws IOException {
        // Audit hard delete
        userAuditRepository.save(UserAudit.builder()
                .userId(user.getId())
                .username(user.getUsername())
                .role(user.getRole() != null ? user.getRole().name() : null)
                .action("HARD_DELETE")
                .reason("Permanent deletion of user")
                .performedById(performedById)
                .performedByUsername(performedByUsername)
                .timestamp(LocalDateTime.now())
                .build());

        // Audit images
        if (user.getImages() != null) {
            user.getImages().forEach(image -> {
                userImageAuditRepository.save(UserImageAudit.builder()
                        .userId(user.getId())
                        .username(user.getUsername())
                        .fileName(image.getFileName())
                        .filePath(image.getFilePath())
                        .action("HARD_DELETE")
                        .reason("Permanent deletion due to user hard delete")
                        .performedById(performedById)
                        .performedByUsername(performedByUsername)
                        .timestamp(LocalDateTime.now())
                        .build());
            });
        }

        // Delete files from disk
        Path userDir = fileStorageService.userRoot()
                .resolve(user.getUploadFolder())
                .normalize();

        // Delete the user
        userDepartmentRepository.deleteByUserId(tenantId(), user.getId());
        userBranchRepository.deleteByUserId(tenantId(), user.getId());
        userRepository.delete(user);

        if (!userDir.startsWith(fileStorageService.userRoot())) {
            throw new SecurityException("Invalid directory path");
        }

        userImageService.deleteUserUploadDirectory(userDir);

        return Map.of("username", user.getUsername());
    }


    /* ====================== HELPER METHODS ====================== */

    public void evictUsersAfterCommit() {
        TransactionSynchronizationManager.registerSynchronization(
                new TransactionSynchronization() {
                    @Override
                    public void afterCommit() {
                        cacheInvalidationService.evictUsers(tenantId());
                    }
                }
        );
    }

    private void validateManagementAction(
            User actor,
            User target,
            boolean allowSelf
    ) {
        if (!privilegesChecker.isAuthorizedWithinDepartment(actor, target)) {
            throw new UnauthorizedAccessException(
                    "Not authorized to manage user: " + target.getUsername()
            );
        }

        if (!allowSelf && actor.getId().equals(target.getId())) {
            throw new UnauthorizedAccessException(
                    "Self destructive operations prohibited"
            );
        }
    }

    private void enforceSingleProfileThumbnail(
            User user,
            String incomingDescription
    ) {

        if (incomingDescription == null ||
                !incomingDescription.trim()
                        .equalsIgnoreCase("profile-thumbnail")) {
            return;
        }

        List<UserImage> existing =
                userImageRepository.findByUser(user);

        existing.stream()
                .filter(i -> !Boolean.TRUE.equals(i.getDeleted()))
                .filter(i ->
                        i.getFileDescription() != null &&
                                i.getFileDescription()
                                        .trim()
                                        .equalsIgnoreCase("profile-thumbnail")
                )
                .forEach(i -> {
                    // demote old thumbnail back to normal image
                    i.setFileDescription("passport");
                    userImageRepository.save(i);
                });
    }

    /* =====================================================
   SOFT DELETE OLD IMAGES
   ===================================================== */
    private void softDeleteAllUserImages(User target, Path userDir, Authentication auth) {

        List<UserImage> images = target.getImages();
        if (images == null || images.isEmpty()) return;

        // soft delete each image
        for (UserImage img : images) {
            img.setDeleted(true);
            userImageRepository.save(img);
        }

        // single audit entry for all deletions
        userImageAuditRepository.save(UserImageAudit.builder()
                .userId(target.getId())
                .username(target.getUsername())
                .fileName("ALL")
                .filePath(userDir.toString())
                .action("SOFT_DELETED_ALL")
                .reason("Inactivated during user update")
                .performedById(privilegesChecker.getAuthenticatedUser(auth).getId())
                .performedByUsername(privilegesChecker.getAuthenticatedUser(auth).getUsername())
                .timestamp(LocalDateTime.now())
                .build());
    }

    /* =====================================================
   SAVE NEW IMAGES
   ===================================================== */
    private void saveNewUserImages(User target,
                                   List<FIleUploadDTO> newFiles,
                                   Authentication auth) throws IOException {

        Map<String, String> uploadedUrls = userImageService.saveFiles(
                newFiles,
                target.getUploadFolder()
        );

        for (String url : uploadedUrls.keySet()) {
            UUID imageBranchId =
                    target.getBranches()
                            .stream()
                            .filter(UserBranch::isPrimaryBranch)
                            .map(ub -> ub.getBranch().getId())
                            .findFirst()
                            .orElseThrow();
            String description =
                    uploadedUrls.get(url);

            enforceSingleProfileThumbnail(
                    target,
                    description
            );

            UserImage img = UserImage.builder()
                    .fileName(Paths.get(url).getFileName().toString())
                    .filePath(url)
                    .fileDescription(description)
                    .user(target)
                    .uploadedAt(LocalDateTime.now())
                    .deleted(false)
                    .branchId(imageBranchId)
                    .build();

            userImageRepository.save(img);

            // audit per image
            userImageAuditRepository.save(UserImageAudit.builder()
                    .userId(target.getId())
                    .username(target.getUsername())
                    .fileName(img.getFileName())
                    .filePath(img.getFilePath())
                    .action("UPLOAD")
                    .performedById(privilegesChecker.getAuthenticatedUser(auth).getId())
                    .performedByUsername(privilegesChecker.getAuthenticatedUser(auth).getUsername())
                    .timestamp(LocalDateTime.now())
                    .build());
        }
    }


    private void checkEmailUniqueness(Set<String> emails) {
        UUID tenantId = TenantContext.getTenantId();
        for (String email : emails) {
            if (userRepository.existsByEmailAddressAndTenantId(email, tenantId)) {
                throw new InvalidUserDataException(
                        "Email already in use: " + email
                );
            }
        }
    }

    private void checkEmailUniqueness(List<String> emails, UUID currentUserId, List<String> oldEmails) {
        for (String email : emails) {
            if (!oldEmails.contains(email)) {
                userRepository.findByEmailElementIgnoreCase(email, tenantId())
                        .filter(u -> !u.getId().equals(currentUserId))
                        .ifPresent(u -> {
                            throw new InvalidUserDataException("Email already in use: " + email);
                        });
            }
        }
    }

    private void checkPhoneUniqueness(Set<String> phones) {
        UUID tenantId = TenantContext.getTenantId();
        for (String phone : phones) {
            if (userRepository.existsByPhoneNumberAndTenantId(phone, tenantId)) {
                throw new InvalidUserDataException(
                        "Phone number already in use: " + phone
                );
            }
        }
    }

    private void checkPhoneUniqueness(List<String> phones, UUID currentUserId, List<String> oldPhones) {
        for (String phone : phones) {
            if (!oldPhones.contains(phone)) {
                userRepository.findByPhoneNumberElement(phone, tenantId())
                        .filter(u -> !u.getId().equals(currentUserId))
                        .ifPresent(u -> {
                            throw new InvalidUserDataException("Phone number already in use: " + phone);
                        });
            }
        }
    }

    /**
     * Apply default MAIN branch + GENERAL department if the user was created without assignments.
     */
    /**
     * Applies MAIN + GENERAL ONLY if the DTO contains NO assignments.
     */
    private boolean applyDefaultBranchAndDepartmentIfMissing(
            UserDTO dto,
            User user
    ) {

        boolean noAssignments =
                dto.getDepartmentsAndPositions() == null ||
                        dto.getDepartmentsAndPositions().isEmpty();

        if (!noAssignments) {
            return false;
        }

        Branch main = branchRepository
                .findByTenantIdAndBranchCodeIgnoreCaseAndDeletedFalse(
                        tenantId(),
                        "MAIN"
                )
                .orElseThrow(() -> new RuntimeException("Default MAIN branch missing"));

        Department general = departmentRepository
                .findByTenantIdAndNameIgnoreCaseAndBranch_Id(
                        tenantId(),
                        "GENERAL",
                        main.getId()
                )
                .orElseThrow(() -> new RuntimeException("Default GENERAL department missing"));

        if (!general.getBranch().getId().equals(main.getId())) {
            throw new IllegalStateException("GENERAL department must belong to MAIN branch");
        }

        /* ---------- enforce single primary branch ---------- */

        userBranchRepository.findByUserId(tenantId(), user.getId())
                .forEach(rel -> rel.setPrimaryBranch(false));

        /* ---------- branch relation ---------- */

        if (!userBranchRepository.existsByUser_IdAndBranch_Id(user.getId(), main.getId())) {

            userBranchRepository.save(
                    UserBranch.builder()
                            .id(new UserBranchId(user.getId(), main.getId()))
                            .user(user)
                            .branch(main)
                            .primaryBranch(true)
                            .build()
            );
        }

        /* ---------- enforce single primary department ---------- */

        userDepartmentRepository.findByUserId(tenantId(), user.getId())
                .forEach(rel -> rel.setPrimaryDepartment(false));

        /* ---------- department relation ---------- */

        if (!userDepartmentRepository
                .existsByUser_IdAndDepartment_Id(user.getId(), general.getId())) {

            userDepartmentRepository.save(
                    UserDepartment.builder()
                            .id(new UserDepartmentId(user.getId(), general.getId()))
                            .user(user)
                            .department(general)
                            .role(DepartmentMembershipRole.MEMBER)
                            .primaryDepartment(true)
                            .build()
            );
        }

        log.info(
                "User '{}' automatically assigned to MAIN → GENERAL",
                user.getUsername()
        );

        return true;
    }

    private void applyDepartmentAndBranchAssignments(
            UserDTO dto,
            User user,
            Authentication authentication
    ) {

        if (dto.getDepartmentsAndPositions() == null ||
                dto.getDepartmentsAndPositions().isEmpty()) {
            return;
        }

        for (DepartmentAssignmentDTO assignment : dto.getDepartmentsAndPositions()) {

            UUID branchId = assignment.getBranchId();
            UUID departmentId = assignment.getDepartmentId();

            Department department =
                    departmentRepository
                            .findByTenantIdAndIdAndDeletedFalse(
                                    tenantId(),
                                    departmentId
                            )
                            .orElseThrow(() -> new EntityNotFoundException("Department not found"));

            if (!department.getBranch().getId().equals(branchId)) {
                throw new IllegalArgumentException(
                        "Department does not belong to selected branch"
                );
            }

            DepartmentMembershipRole role =
                    assignment.getPosition().equalsIgnoreCase("head")
                            ? DepartmentMembershipRole.HEAD
                            : DepartmentMembershipRole.MEMBER;

            /* ---------- department relation ---------- */

            if (!userDepartmentRepository
                    .existsByUser_IdAndDepartment_Id(user.getId(), departmentId)) {

                userDepartmentRepository.save(
                        UserDepartment.builder()
                                .id(new UserDepartmentId(user.getId(), departmentId))
                                .user(user)
                                .department(department)
                                .role(role)
                                .primaryDepartment(false)
                                .build()
                );
            }

            /* ---------- branch relation ---------- */

            if (!userBranchRepository
                    .existsByUser_IdAndBranch_Id(user.getId(), branchId)) {

                userBranchRepository.save(
                        UserBranch.builder()
                                .id(new UserBranchId(user.getId(), branchId))
                                .user(user)
                                .branch(department.getBranch())
                                .primaryBranch(false)
                                .build()
                );
            }
        }
    }

    private void uploadUserImages(List<FIleUploadDTO> files, String uploadFolder, User user,
                                  UUID creatorId, String creatorUsername) throws IOException {
        if (files == null || files.isEmpty()) return;

        Path userUploadDir = fileStorageService.userRoot()
                .resolve(uploadFolder)
                .normalize();

        Map<String, String> apiUrls = userImageService.saveFiles(files, uploadFolder);

        for (String apiUrl : apiUrls.keySet()) {
            // Extract the filename from API URL
            String fileName = Paths.get(apiUrl).getFileName().toString();

            Path physicalFile = userUploadDir.resolve(fileName);

            // Persist UserImage in DB with API-friendly URL
            UUID imageBranchId =
                    userBranchRepository
                            .findByUserId(tenantId(), user.getId())
                            .stream()
                            .filter(UserBranch::isPrimaryBranch)
                            .map(ub -> ub.getBranch().getId())
                            .findFirst()
                            .orElseThrow(() ->
                                    new IllegalStateException("User has no primary branch assigned")
                            );

            String description = apiUrls.get(apiUrl);

            enforceSingleProfileThumbnail(
                    user,
                    description
            );

            UserImage image = UserImage.builder()
                    .fileName(fileName)
                    .filePath(apiUrl)
                    .fileDescription(description)
                    .user(user)
                    .uploadedAt(LocalDateTime.now())
                    .deleted(false)
                    .branchId(imageBranchId)
                    .build();
            userImageRepository.save(image);

            // Audit image upload
            userImageAuditRepository.save(UserImageAudit.builder()
                    .userId(user.getId())
                    .username(user.getUsername())
                    .fileName(fileName)
                    .filePath(apiUrl)
                    .action("UPLOAD")
                    .performedById(creatorId)
                    .performedByUsername(creatorUsername)
                    .timestamp(LocalDateTime.now())
                    .build());
        }
    }

    private void auditChanges(User user, Map<String, String[]> changes, String updaterUsername) {
        UUID updaterId = userRepository
                .findByUsernameAndTenantId(updaterUsername, tenantId())
                .map(User::getId)
                .orElse(null);

        for (var entry : changes.entrySet()) {
            userAuditRepository.save(UserAudit.builder()
                    .userId(user.getId())
                    .username(user.getUsername())
                    .fieldChanged(entry.getKey())
                    .oldValue(entry.getValue()[0])
                    .newValue(entry.getValue()[1])
                    .action("UPDATE")
                    .performedById(updaterId)
                    .performedByUsername(updaterUsername)
                    .timestamp(LocalDateTime.now())
                    .build());
        }
    }

    private void updateEmailsAndAudit(User user, List<String> oldEmails, List<String> newEmails, String updaterUsername) {
        List<String> added = newEmails.stream().filter(e -> !oldEmails.contains(e)).toList();
        List<String> removed = oldEmails.stream().filter(e -> !newEmails.contains(e)).toList();

        if (!added.isEmpty() || !removed.isEmpty()) user.setEmailAddresses(newEmails);

        UUID updaterId = userRepository
                .findByUsernameAndTenantId(updaterUsername, tenantId())
                .map(User::getId)
                .orElse(null);

        for (String a : added) auditEmailChange(user, "email_add", null, a, updaterId, updaterUsername);
        for (String r : removed) auditEmailChange(user, "email_remove", r, null, updaterId, updaterUsername);
    }

    private void updatePhonesAndAudit(User user, List<String> oldPhones, List<String> newPhones, String updaterUsername) {
        List<String> added = newPhones.stream().filter(p -> !oldPhones.contains(p)).toList();
        List<String> removed = oldPhones.stream().filter(p -> !newPhones.contains(p)).toList();

        if (!added.isEmpty() || !removed.isEmpty()) user.setPhoneNumbers(newPhones);

        UUID updaterId = userRepository
                .findByUsernameAndTenantId(updaterUsername, tenantId())
                .map(User::getId)
                .orElse(null);

        for (String a : added) auditPhoneChange(user, "phone_add", null, a, updaterId, updaterUsername);
        for (String r : removed) auditPhoneChange(user, "phone_remove", r, null, updaterId, updaterUsername);
    }

    private void auditEmailChange(User user, String field, String oldValue, String newValue, UUID updaterId, String updaterUsername) {
        userAuditRepository.save(UserAudit.builder()
                .userId(user.getId())
                .username(user.getUsername())
                .fieldChanged(field)
                .oldValue(oldValue)
                .newValue(newValue)
                .action("UPDATE")
                .performedById(updaterId)
                .performedByUsername(updaterUsername)
                .timestamp(LocalDateTime.now())
                .build());
    }

    private void auditPhoneChange(User user, String field, String oldValue, String newValue, UUID updaterId, String updaterUsername) {
        userAuditRepository.save(UserAudit.builder()
                .userId(user.getId())
                .username(user.getUsername())
                .fieldChanged(field)
                .oldValue(oldValue)
                .newValue(newValue)
                .action("UPDATE")
                .performedById(updaterId)
                .performedByUsername(updaterUsername)
                .timestamp(LocalDateTime.now())
                .build());
    }

    private UserDTO mapToDTO(User user) {

        UserDTO dto = UserDTO.builder()
                .id(user.getId())
                .username(user.getUsername())
                .emailAddresses(
                        user.getEmailAddresses() == null
                                ? List.of()
                                : new ArrayList<>(user.getEmailAddresses())
                )
                .phoneNumbers(
                        user.getPhoneNumbers() == null
                                ? List.of()
                                : new ArrayList<>(user.getPhoneNumbers())
                )
                .idNumber(user.getIdNumber())
                .role(user.getRole() != null ? user.getRole().name() : null)
                .deleted(Boolean.TRUE.equals(user.getDeleted()))
                .createdAt(user.getCreatedAt())
                .profileThumbnailUrl(
                        UserImageMapper.resolveProfileThumbnail(
                                user.getImages()
                        )
                )
                .build();

        Map<Branch, List<DepartmentPositionDTO>> grouped = new HashMap<>();

        if (user.getDepartments() != null) {

            for (UserDepartment rel : user.getDepartments()) {

                Department dept = rel.getDepartment();
                Branch branch = dept.getBranch();

                grouped.computeIfAbsent(branch, b -> new ArrayList<>())
                        .add(
                                DepartmentPositionDTO.builder()
                                        .departmentId(dept.getId())
                                        .departmentName(dept.getName())
                                        .position(rel.getRole().name().toLowerCase())
                                        .build()
                        );
            }
        }

        List<BranchHierarchyDTO> hierarchy =
                grouped.entrySet().stream()
                        .map(entry ->
                                BranchHierarchyDTO.builder()
                                        .branchId(entry.getKey().getId())
                                        .branchName(entry.getKey().getName())
                                        .departments(entry.getValue())
                                        .build()
                        )
                        .toList();

        dto.setBranchHierarchy(hierarchy);

        return dto;
    }
}