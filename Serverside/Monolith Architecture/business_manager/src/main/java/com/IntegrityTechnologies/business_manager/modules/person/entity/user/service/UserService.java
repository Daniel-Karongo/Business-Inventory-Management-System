package com.IntegrityTechnologies.business_manager.modules.person.entity.user.service;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import com.IntegrityTechnologies.business_manager.common.FIleUploadDTO;
import com.IntegrityTechnologies.business_manager.common.PhoneAndEmailNormalizer;
import com.IntegrityTechnologies.business_manager.common.PrivilegesChecker;
import com.IntegrityTechnologies.business_manager.config.FileStorageProperties;
import com.IntegrityTechnologies.business_manager.config.FileStorageService;
import com.IntegrityTechnologies.business_manager.config.TransactionalFileManager;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.exception.InvalidUserDataException;
import com.IntegrityTechnologies.business_manager.exception.UnauthorizedAccessException;
import com.IntegrityTechnologies.business_manager.exception.UserNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.dto.BranchHierarchyDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.service.BranchService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.dto.DepartmentAssignmentDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.dto.DepartmentPositionDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.service.DepartmentService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.dto.UserDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserImageAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserImageRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.io.IOException;
import java.nio.file.Files;
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
    private final FileStorageProperties fileStorageProperties;
    private final UserImageService userImageService;
    private final PrivilegesChecker privilegesChecker;
    private final FileStorageService fileStorageService;
    private final TransactionalFileManager transactionalFileManager;
    private final DepartmentRepository departmentRepository;
    private final DepartmentService departmentService;
    private final BranchRepository branchRepository;
    private final BranchService branchService;

    /* ====================== REGISTER USER ====================== */
    @Transactional
    public UserDTO registerUser(UserDTO dto, Authentication authentication) throws IOException {
        System.out.println("Hello");
        System.out.println(dto);

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

        if (userRepository.existsByUsername(dto.getUsername()))
            throw new InvalidUserDataException("Username already in use");

        if (dto.getIdNumber() != null && userRepository.existsByIdNumber(dto.getIdNumber()))
            throw new InvalidUserDataException("ID number already in use");

        // 2️⃣ Resolve creator
        User creator = userRepository.findByUsername(creatorUsername).orElse(null);
        Role newUserRole = Role.valueOf(dto.getRole().trim().toUpperCase());

        if (creator != null) {
            Role creatorRole = creator.getRole();

            if (!creatorRole.canAccess(newUserRole)) {
                throw new UnauthorizedAccessException(
                        "You cannot create a user with role " + newUserRole +
                                " because your role is " + creatorRole
                );
            }
        }

        // 3️⃣ Generate upload folder upfront
        String uploadFolder = UUID.randomUUID() + "_" + System.currentTimeMillis();

        // 4️⃣ Build & save user
        User user = User.builder()
                .username(dto.getUsername())
                .password(dto.getPassword() != null ? passwordEncoder.encode(dto.getPassword()) : passwordEncoder.encode(""))
                .emailAddresses(new ArrayList<>(emails))
                .phoneNumbers(new ArrayList<>(phones))
                .idNumber(dto.getIdNumber())
                .role(Role.valueOf(dto.getRole().trim().toUpperCase()))
                .deleted(false)
                .uploadFolder(uploadFolder) // ✅ set before save
                .createdBy(creator)
                .build();

        user = userRepository.save(user);

        boolean assignedDefaults = applyDefaultBranchAndDepartmentIfMissing(dto, user);

        if (!assignedDefaults) {
            // Only apply admin assignments when defaults were NOT used
            applyDepartmentAndBranchAssignments(dto, user, authentication);
        }

        // 5️⃣ Create actual folder on disk
        Path baseUserUploadDir = Paths.get(fileStorageProperties.getUserUploadDir());
        Path userFolderPath = baseUserUploadDir.resolve(uploadFolder);
        // Create directories if they don't exist
        if (!Files.exists(userFolderPath)) Files.createDirectories(userFolderPath);
        fileStorageService.hidePath(userFolderPath);
        // Track folder for rollback cleanup
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

        return mapToDTO(user, false);
    }


    /* ====================== UPDATE USER ====================== */
    @Transactional
    public UserDTO updateUser(String identifier, UserDTO updatedData, Authentication authentication) throws IOException {
        String updaterUsername = (authentication != null && authentication.getPrincipal() instanceof UserDetails userDetails)
                ? userDetails.getUsername()
                : null;

        // 1️⃣ Lookup target user
        User user = userRepository.findByIdentifier(identifier)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        // 2️⃣ Lookup updater
        User updater = userRepository.findByIdentifier(updaterUsername)
                .orElseThrow(() -> new UserNotFoundException("Updater not found"));

        // 3️⃣ Validate access using centralized method
        if (!privilegesChecker.isAuthorized(updater, user)) {
            throw new UnauthorizedAccessException(
                    "You are not authorized to update user: " + user.getUsername()
            );
        }

        Map<String, String[]> changes = new HashMap<>();

        // 4️⃣ Username
        if (updatedData.getUsername() != null && !updatedData.getUsername().isBlank()
                && !updatedData.getUsername().equals(user.getUsername())) {
            if (userRepository.existsByUsername(updatedData.getUsername()))
                throw new InvalidUserDataException("Username already in use");
            changes.put("username", new String[]{user.getUsername(), updatedData.getUsername()});
            user.setUsername(updatedData.getUsername());
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
            if (userRepository.existsByIdNumber(updatedData.getIdNumber()))
                throw new InvalidUserDataException("ID number already in use");
            changes.put("idNumber", new String[]{user.getIdNumber(), updatedData.getIdNumber()});
            user.setIdNumber(updatedData.getIdNumber());
        }

        // 8️⃣ Password
        if (updatedData.getPassword() != null && !updatedData.getPassword().isBlank()) {
            user.setPassword(passwordEncoder.encode(updatedData.getPassword()));
        }

        // 9️⃣ Role
        if (updatedData.getRole() != null) {
            if (user.getUsername().equals(updaterUsername))
                throw new UnauthorizedAccessException("You cannot change your own role");

            Role newRole = Role.valueOf(updatedData.getRole().toUpperCase());
            if (!updater.getRole().canAccess(newRole)) {
                throw new UnauthorizedAccessException("You are not authorized to assign this role");
            }

            changes.put("role", new String[]{user.getRole().name(), newRole.name()});
            user.setRole(newRole);
        }

        // 1️⃣0️⃣ Save and audit
        user = userRepository.save(user);

        applyDepartmentAndBranchAssignments(updatedData, user, authentication);

        auditChanges(user, changes, updaterUsername);

        return mapToDTO(user, false);
    }

    @Transactional
    public ResponseEntity<?> updateUserImages(
            String identifier,
            List<FIleUploadDTO> newFiles,
            Authentication auth,
            Boolean deleteOldImages
    ) throws IOException {

        User target = validateAccess(identifier, auth, "update images");

        Path userDir = Paths.get(
                fileStorageProperties.getUserUploadDir(),
                target.getUploadFolder()
        ).toAbsolutePath().normalize();

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

        return ResponseEntity.ok(mapToDTO(target, false));
    }

    /* ====================== IMAGE & USER ACCESS VALIDATION ====================== */
    private User validateAccess(String identifier, Authentication auth, String action) {
        User target = userRepository.findByIdentifier(identifier)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        User requester = privilegesChecker.getAuthenticatedUser(auth);
        if (!privilegesChecker.isAuthorized(requester, target)) {
            throw new UnauthorizedAccessException("You are not authorized to " + action + " for user: " + target.getUsername());
        }
        return target;
    }

    private boolean hasManagerialRole(String username) {
        return userRepository.findByUsername(username)
                .map(user -> user.getRole() == Role.SUPERUSER ||user.getRole() == Role.ADMIN || user.getRole() == Role.MANAGER)
                .orElse(false);
    }

    /* ====================== GET USERS ====================== */

    public UserDTO getUser(String identifier, Boolean deleted)  {
        User user;

        if(Boolean.FALSE.equals(deleted)) {
             user = userRepository.findByIdentifier(identifier)
                    .orElseThrow(() -> new UserNotFoundException("User not found: " + identifier));
            return mapToDTO(user, false);
        } else {
            user = userRepository.findByIdentifierForAudits(identifier)
                    .orElseThrow(() -> new UserNotFoundException("User not found: " + identifier));
            return mapToDTO(user, null);
        }

    }

    public List<UserDTO> getAllUsers(Boolean deleted) {
        List<User> users = new ArrayList<>();
        if(Boolean.FALSE.equals(deleted)) {
            users = userRepository.findByDeletedFalse();
            return users.stream()
                    .map(user -> mapToDTO(user, false)) // false = only active images
                    .toList();
        } else if(Boolean.TRUE.equals(deleted)) {
            users = userRepository.findByDeletedTrue();
            return users.stream()
                    .map(user -> mapToDTO(user, false))
                    .toList();
        } else {
            users = userRepository.findAll();
            return users.stream()
                    .map(user -> mapToDTO(user, null))
                    .toList();
        }
    }

    public List<UserDTO> getUsersByRole(Role requestedRole, Boolean deleted) {
        try {
            Role currentUserRole = privilegesChecker.getCurrentUserRole();
            if (!currentUserRole.canAccess(requestedRole)) {
                throw new UnauthorizedAccessException("You cannot access users of this role.");
            }
            List<User> users = new ArrayList<>();
            if(Boolean.FALSE.equals(deleted))
                users = userRepository.findActiveUsersByRole(requestedRole);
            else if(Boolean.TRUE.equals(deleted))
                users = userRepository.findDeletedUsersByRole(requestedRole);
            else
                users = userRepository.findAllUsersByRole(requestedRole);

            List<UserDTO> dtos = users.stream()
                    .map(user -> mapToDTO(user, false))
                    .toList();
            return dtos;
        } catch (IllegalArgumentException e) {
            throw new InvalidUserDataException("Invalid role: " + requestedRole.name());
        }
    }

    public User getUserByIdentifierForAudits(String identifier) {
        return userRepository.findByIdentifierForAudits(identifier)
                .orElseThrow(() -> new UserNotFoundException("User not found"));
    }

    public List<UserAudit> getUserAuditsTarget(UUID userId) {
        return userAuditRepository.findByUserIdOrderByTimestampDesc(userId);
    }
    public List<UserAudit> getUserAuditsPerpetrated(UUID userId) {
        return userAuditRepository.findByPerformedByIdOrderByTimestampDesc(userId);
    }

    public List<UserImageAudit> getUserImageAuditsTarget(UUID userId) {
        return userImageAuditRepository.findByUserIdOrderByTimestampDesc(userId);
    }
    public List<UserImageAudit> getUserImageAuditsPerpetrated(UUID userId) {
        return userImageAuditRepository.findByPerformedByIdOrderByTimestampDesc(userId);
    }


    /* ====================== SOFT DELETE / RESTORE ====================== */
    // ====================== SOFT DELETE ======================

    @Transactional
    public ResponseEntity<ApiResponse> softDeleteUser(UUID id, Authentication authentication, String reason) {
        User user = userRepository.findById(id)
                .orElseThrow(() -> new UserNotFoundException("User not found: " + id));

        UUID performedById = privilegesChecker.getAuthenticatedUser(authentication).getId();
        String performedByUsername = privilegesChecker.getAuthenticatedUser(authentication).getUsername();

        Map<String, Object> modified = softDeleteUserInternal(user, performedById, performedByUsername, reason);

        return ResponseEntity.ok(new ApiResponse(
                "success",
                "User soft deleted successfully",
                List.of(modified)
        ));
    }

    @Transactional
    public ResponseEntity<ApiResponse> softDeleteUsersInBulk(List<UUID> userIds, String reason, Authentication authentication) {
        UUID performedById = privilegesChecker.getAuthenticatedUser(authentication).getId();
        String performedByUsername = privilegesChecker.getAuthenticatedUser(authentication).getUsername();

        List<Map<String, Object>> modifiedUsers = new ArrayList<>();
        for (UUID id : userIds) {
            User user = userRepository.findById(id)
                    .orElseThrow(() -> new UserNotFoundException("User not found: " + id));
            modifiedUsers.add(softDeleteUserInternal(user, performedById, performedByUsername, reason));
        }

        return ResponseEntity.ok(new ApiResponse(
                "success",
                "Users soft deleted successfully",
                modifiedUsers
        ));
    }

    /** Internal helper for soft delete */
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
        User user = userRepository.findById(id)
                .orElseThrow(() -> new UserNotFoundException("User not found: " + id));

        UUID performedById = privilegesChecker.getAuthenticatedUser(authentication).getId();
        String performedByUsername = privilegesChecker.getAuthenticatedUser(authentication).getUsername();

        Map<String, Object> restored = restoreUserInternal(user, performedById, performedByUsername, reason);

        return ResponseEntity.ok(new ApiResponse(
                "success",
                "User restored successfully",
                List.of(restored)
        ));
    }

    @Transactional
    public ResponseEntity<ApiResponse> restoreUsersInBulk(List<UUID> userIds, String reason, Authentication authentication) throws IOException {
        UUID performedById = privilegesChecker.getAuthenticatedUser(authentication).getId();
        String performedByUsername = privilegesChecker.getAuthenticatedUser(authentication).getUsername();

        List<Map<String, Object>> restoredUsers = new ArrayList<>();
        for (UUID id : userIds) {
            User user = userRepository.findById(id)
                    .orElseThrow(() -> new UserNotFoundException("User not found: " + id));
            restoredUsers.add(restoreUserInternal(user, performedById, performedByUsername, reason));
        }

        return ResponseEntity.ok(new ApiResponse(
                "success",
                "Users restored successfully",
                restoredUsers
        ));
    }

    /** Internal helper for restore */
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

//        // Hide images
//        Path userDir = Paths.get(fileStorageProperties.getUserUploadDir(), user.getUploadFolder())
//                .toAbsolutePath().normalize();
//        if (Files.exists(userDir)) {
//            Files.walk(userDir)
//                    .filter(Files::isRegularFile)
//                    .forEach(path -> {
//                        try { fileStorageService.hidePath(path); } catch (IOException e) { log.warn("Failed to unhide file: {}", path, e); }
//                    });
//            fileStorageService.hidePath(userDir);
//        }

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
        User user = userRepository.findById(id)
                .orElseThrow(() -> new UserNotFoundException("User not found: " + id));

        UUID performedById = privilegesChecker.getAuthenticatedUser(authentication).getId();
        String performedByUsername = privilegesChecker.getAuthenticatedUser(authentication).getUsername();

        Map<String, Object> deleted = hardDeleteUserInternal(user, performedById, performedByUsername);

        return ResponseEntity.ok(new ApiResponse(
                "success",
                "User permanently deleted",
                List.of(deleted)
        ));
    }

    @Transactional
    public ResponseEntity<ApiResponse> hardDeleteUsersInBulk(List<UUID> userIds, Authentication authentication) throws IOException {
        UUID performedById = privilegesChecker.getAuthenticatedUser(authentication).getId();
        String performedByUsername = privilegesChecker.getAuthenticatedUser(authentication).getUsername();

        List<Map<String, Object>> deletedUsers = new ArrayList<>();
        for (UUID id : userIds) {
            User user = userRepository.findById(id)
                    .orElseThrow(() -> new UserNotFoundException("User not found: " + id));
            deletedUsers.add(hardDeleteUserInternal(user, performedById, performedByUsername));
        }

        return ResponseEntity.ok(new ApiResponse(
                "success",
                "Users permanently deleted",
                deletedUsers
        ));
    }

    /** Internal helper for hard delete */
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
        Path userDir = Paths.get(fileStorageProperties.getUserUploadDir(), user.getUploadFolder())
                .toAbsolutePath().normalize();

        // Delete the user
        userRepository.deleteUserFromBranch(user.getId());
        userRepository.deleteUserFromDepartmentHeads(user.getId());
        userRepository.deleteUserFromDepartmentMembers(user.getId());
        userRepository.delete(user);

        userImageService.deleteUserUploadDirectory(userDir);

        return Map.of("username", user.getUsername());
    }


    /* ====================== HELPER METHODS ====================== */

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
                target.getUploadFolder(),
                fileStorageProperties.getUserUploadDir()
        );

        for (String url : uploadedUrls.keySet()) {
            UserImage img = UserImage.builder()
                    .fileName(Paths.get(url).getFileName().toString())
                    .filePath(url)
                    .fileDescription(uploadedUrls.get(url))
                    .user(target)
                    .uploadedAt(LocalDateTime.now())
                    .deleted(false)
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
        for (String email : emails) {
            if (userRepository.existsByEmailAddress(email))
                throw new InvalidUserDataException("Email already in use: " + email);
        }
    }

    private void checkEmailUniqueness(List<String> emails, UUID currentUserId, List<String> oldEmails) {
        for (String email : emails) {
            if (!oldEmails.contains(email)) {
                userRepository.findByEmailElementIgnoreCase(email)
                        .filter(u -> !u.getId().equals(currentUserId))
                        .ifPresent(u -> { throw new InvalidUserDataException("Email already in use: " + email); });
            }
        }
    }

    private void checkPhoneUniqueness(Set<String> phones) {
        for (String phone : phones) {
            if (userRepository.existsByPhoneNumber(phone))
                throw new InvalidUserDataException("Phone number already in use: " + phone);
        }
    }

    private void checkPhoneUniqueness(List<String> phones, UUID currentUserId, List<String> oldPhones) {
        for (String phone : phones) {
            if (!oldPhones.contains(phone)) {
                userRepository.findByPhoneNumberElement(phone)
                        .filter(u -> !u.getId().equals(currentUserId))
                        .ifPresent(u -> { throw new InvalidUserDataException("Phone number already in use: " + phone); });
            }
        }
    }

    /**
     * Apply default MAIN branch + GENERAL department if the user was created without assignments.
     */
    /**
     * Applies MAIN + GENERAL ONLY if the DTO contains NO assignments.
     */
    private boolean applyDefaultBranchAndDepartmentIfMissing(UserDTO dto, User user) {

        boolean noAssignments =
                dto.getDepartmentsAndPositions() == null ||
                        dto.getDepartmentsAndPositions().isEmpty();

        if (!noAssignments) {
            // Admin provided assignments → do NOT apply defaults
            return false;
        }

        // --- Load defaults ---
        Branch main = branchRepository.findByBranchCode("MAIN")
                .orElseThrow(() -> new RuntimeException("Default MAIN branch missing"));

        Department general = departmentRepository.findByNameIgnoreCase("GENERAL")
                .orElseThrow(() -> new RuntimeException("Default GENERAL department missing"));

        // --- Link GENERAL to MAIN if needed ---
        if (!main.getDepartments().contains(general)) {
            main.getDepartments().add(general);
            branchRepository.save(main);
        }

        // --- Assign user to MAIN branch ---
        if (!main.getUsers().contains(user)) {
            main.getUsers().add(user);
            branchRepository.save(main);
        }

        // --- Assign user to GENERAL department (as MEMBER) ---
        if (!general.getMembers().contains(user)) {
            general.addMember(user);
            departmentRepository.save(general);
        }

        log.info("User '{}' automatically assigned to MAIN → GENERAL", user.getUsername());
        return true;  // <--- important
    }

    private void applyDepartmentAndBranchAssignments(UserDTO dto, User user, Authentication authentication) {

        if (dto.getDepartmentsAndPositions() == null || dto.getDepartmentsAndPositions().isEmpty()) {
            return;
        }

        // 1️⃣ Validate head/member conflicts per department
        Map<UUID, Set<String>> departmentRolesMap = new HashMap<>();

        for (DepartmentAssignmentDTO a : dto.getDepartmentsAndPositions()) {
            departmentRolesMap
                    .computeIfAbsent(a.getDepartmentId(), k -> new HashSet<>())
                    .add(a.getPosition().toLowerCase());
        }

        for (Map.Entry<UUID, Set<String>> entry : departmentRolesMap.entrySet()) {
            Set<String> roles = entry.getValue();
            if (roles.contains("head") && roles.contains("member")) {
                Department d = departmentRepository.findById(entry.getKey()).orElse(null);
                String name = d != null ? d.getName() : entry.getKey().toString();
                throw new IllegalArgumentException("User cannot be both head and member in department: " + name);
            }
        }

        // 2️⃣ Process each assignment
        for (DepartmentAssignmentDTO assignment : dto.getDepartmentsAndPositions()) {

            UUID branchId = assignment.getBranchId();
            UUID deptId = assignment.getDepartmentId();

            // 2.1 Validate branch exists
            Branch branch = branchRepository.findByIdAndDeletedFalse(branchId)
                    .orElseThrow(() ->
                            new EntityNotFoundException("Branch not found: " + branchId));

            // 2.2 Validate department exists
            Department department = departmentRepository.findByIdAndDeletedFalse(deptId)
                    .orElseThrow(() ->
                            new EntityNotFoundException("Department not found: " + deptId));

            // 2.3 Validate that department belongs to branch
            boolean valid = branchRepository.branchContainsDepartment(branchId, deptId);
            if (!valid) {
                throw new IllegalArgumentException(
                        "Department '" + department.getName() + "' does NOT belong to branch '" + branch.getName() + "'"
                );
            }

            // 3️⃣ Ensure user is added to branch if not already there
            if (!branch.getUsers().contains(user)) {
                Set<User> oldUsers = branch.getUsers();
                branch.getUsers().add(user);
                branchRepository.save(branch);

                branchService.recordBranchAudit(
                        branch,
                        "ADD_USER",
                        "users",
                        null,
                        user.getUsername(),
                        authentication,
                        "User added due to department assignment"
                );
            }

            // 4️⃣ Assign department role
            String position = assignment.getPosition().toLowerCase();

            switch (position) {
                case "member" -> {
                    department.addMember(user);
                    departmentService.logAudit(
                            department,
                            "ADD_MEMBER",
                            null,
                            user.getUsername(),
                            authentication,
                            "User added as member"
                    );
                }

                case "head" -> {
                    department.addHead(user);

                    // Auto promote EMPLOYEE → SUPERVISOR
                    if (user.getRole() == Role.EMPLOYEE) {
                        user.setRole(Role.SUPERVISOR);
                        userRepository.save(user);
                    }

                    departmentService.logAudit(
                            department,
                            "ADD_HEAD",
                            null,
                            user.getUsername(),
                            authentication,
                            "User added as department head"
                    );
                }

                default -> throw new IllegalArgumentException(
                        "Invalid position: " + position + " (must be 'head' or 'member')"
                );
            }

            departmentRepository.save(department);
        }
    }

    private void uploadUserImages(List<FIleUploadDTO> files, String uploadFolder, User user,
                                  UUID creatorId, String creatorUsername) throws IOException {
        if (files == null || files.isEmpty()) return;

        // Base upload directory on disk
        Path baseDir = Paths.get(fileStorageProperties.getUserUploadDir()).toAbsolutePath().normalize();
        Path userUploadDir = baseDir.resolve(uploadFolder);

        // Track folder for rollback
        transactionalFileManager.track(userUploadDir);

        // Save files via UserImageService (returns secure API paths)
        Map<String, String> apiUrls = userImageService.saveFiles(files, uploadFolder, fileStorageProperties.getUserUploadDir());

        for (String apiUrl : apiUrls.keySet()) {
            // Extract the filename from API URL
            String fileName = Paths.get(apiUrl).getFileName().toString();

            // Track physical file for rollback
            Path physicalFile = userUploadDir.resolve(fileName);
            transactionalFileManager.track(physicalFile);

            // Persist UserImage in DB with API-friendly URL
            UserImage image = UserImage.builder()
                    .fileName(fileName)
                    .filePath(apiUrl)       // ✅ store API URL, not disk path
                    .fileDescription(apiUrls.get(apiUrl))
                    .user(user)
                    .uploadedAt(LocalDateTime.now())
                    .deleted(false)
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
        UUID updaterId = userRepository.findByUsername(updaterUsername)
                .map(User::getId).orElse(null);

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

        UUID updaterId = userRepository.findByUsername(updaterUsername).map(User::getId).orElse(null);

        for (String a : added) auditEmailChange(user, "email_add", null, a, updaterId, updaterUsername);
        for (String r : removed) auditEmailChange(user, "email_remove", r, null, updaterId, updaterUsername);
    }

    private void updatePhonesAndAudit(User user, List<String> oldPhones, List<String> newPhones, String updaterUsername) {
        List<String> added = newPhones.stream().filter(p -> !oldPhones.contains(p)).toList();
        List<String> removed = oldPhones.stream().filter(p -> !newPhones.contains(p)).toList();

        if (!added.isEmpty() || !removed.isEmpty()) user.setPhoneNumbers(newPhones);

        UUID updaterId = userRepository.findByUsername(updaterUsername).map(User::getId).orElse(null);

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

    private UserDTO mapToDTO(User user, Boolean deleted) {

        UserDTO dto = UserDTO.builder()
                .id(user.getId())
                .username(user.getUsername())
                .emailAddresses(user.getEmailAddresses())
                .phoneNumbers(user.getPhoneNumbers())
                .idNumber(user.getIdNumber())
                .role(user.getRole() != null ? user.getRole().name() : null)
                .createdBy(user.getCreatedBy() != null
                        ? user.getCreatedBy().getId() + " | " + user.getCreatedBy().getUsername()
                        : null)
                .lastModifiedBy(user.getUpdatedBy() != null
                        ? user.getUpdatedBy().getId() + " | " + user.getUpdatedBy().getUsername()
                        : null)
                .createdAt(user.getCreatedAt())
                .lastModifiedAt(user.getUpdatedAt())
                .deleted(Boolean.TRUE.equals(user.getDeleted()))
                .idImageUrls(user.getImages() != null
                        ? user.getImages().stream()
                        .filter(img -> {
                            if (deleted == null) return true;
                            if (deleted) return Boolean.TRUE.equals(img.getDeleted());
                            return !Boolean.TRUE.equals(img.getDeleted());
                        })
                        .map(UserImage::getFilePath)
                        .toList()
                        : List.of())
                .build();

        // --- Build branch hierarchy ---
        List<Branch> branches = branchRepository.findBranchesByUserId(user.getId());
        List<BranchHierarchyDTO> branchHierarchy = branches.stream().map(branch -> {
            List<DepartmentPositionDTO> deptPositions = new ArrayList<>();

            for (Department dept : branch.getDepartments()) {
                String position = null;
                if (dept.getHeads().contains(user)) position = "head";
                else if (dept.getMembers().contains(user)) position = "member";
                else continue; // skip if user is not in this department

                DepartmentPositionDTO deptDto = DepartmentPositionDTO.builder()
                        .departmentId(dept.getId())
                        .departmentName(dept.getName())
                        .position(position)
                        .build();
                deptPositions.add(deptDto);
            }

            return BranchHierarchyDTO.builder()
                    .branchId(branch.getId())
                    .branchName(branch.getName())
                    .departments(deptPositions)
                    .build();
        }).toList();

        dto.setBranchHierarchy(branchHierarchy);

        return dto;
    }
}