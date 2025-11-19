package com.IntegrityTechnologies.business_manager.modules.user.service;

import com.IntegrityTechnologies.business_manager.common.PrivilegesChecker;
import com.IntegrityTechnologies.business_manager.config.FileStorageProperties;
import com.IntegrityTechnologies.business_manager.config.FileStorageService;
import com.IntegrityTechnologies.business_manager.config.TransactionalFileManager;
import com.IntegrityTechnologies.business_manager.exception.*;
import com.IntegrityTechnologies.business_manager.modules.user.dto.UserDTO;
import com.IntegrityTechnologies.business_manager.modules.user.model.*;
import com.IntegrityTechnologies.business_manager.modules.user.repository.*;
import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.Resource;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.nio.file.*;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

import static org.hibernate.query.sqm.tree.SqmNode.log;

@Service
@RequiredArgsConstructor
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

    /* ====================== REGISTER USER ====================== */
    @Transactional
    public UserDTO registerUser(UserDTO dto, String creatorUsername) throws IOException {

        // 0️⃣ Normalize input
        Set<String> emails = normalizeEmails(dto.getEmailAddresses());
        Set<String> phones = normalizePhones(dto.getPhoneNumbers());

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

        // 5️⃣ Create actual folder on disk
        Path baseUserUploadDir = Paths.get(fileStorageProperties.getUserUploadDir());
        Path userFolderPath = baseUserUploadDir.resolve(uploadFolder);
        // Create directories if they don't exist
        if (!Files.exists(userFolderPath)) Files.createDirectories(userFolderPath);
        userImageService.hidePath(userFolderPath);
        // Track folder for rollback cleanup
        transactionalFileManager.track(userFolderPath);


        // 6️⃣ Upload images
        uploadUserImages(dto.getIdImageFiles(), uploadFolder, user,
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

        return mapToDTO(user);
    }


    /* ====================== UPDATE USER ====================== */
    @Transactional
    public UserDTO updateUser(String identifier, UserDTO updatedData, String updaterUsername) throws IOException {
        User user = userRepository.findByIdentifier(identifier)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        if (!updaterUsername.equals(user.getUsername()) && !hasManagerialRole(updaterUsername))
            throw new AccessDeniedException("You are not authorized to update this user.");

        Map<String, String[]> changes = new HashMap<>();

        // 1️⃣ Username
        if (updatedData.getUsername() != null && !updatedData.getUsername().isBlank()
                && !updatedData.getUsername().equals(user.getUsername())) {
            if (userRepository.existsByUsername(updatedData.getUsername()))
                throw new InvalidUserDataException("Username already in use");
            changes.put("username", new String[]{user.getUsername(), updatedData.getUsername()});
            user.setUsername(updatedData.getUsername());
        }

        // 2️⃣ Emails
        List<String> oldEmails = user.getEmailAddresses() != null ? user.getEmailAddresses() : List.of();
        Set<String> newEmailsSet = normalizeEmails(updatedData.getEmailAddresses());
        List<String> newEmails = new ArrayList<>(newEmailsSet);
        checkEmailUniqueness(newEmails, user.getId(), oldEmails);
        updateEmailsAndAudit(user, oldEmails, newEmails, updaterUsername);

        // 3️⃣ Phones
        List<String> oldPhones = user.getPhoneNumbers() != null ? user.getPhoneNumbers() : List.of();
        Set<String> newPhonesSet = normalizePhones(updatedData.getPhoneNumbers());
        List<String> newPhones = new ArrayList<>(newPhonesSet);
        checkPhoneUniqueness(newPhones, user.getId(), oldPhones);
        updatePhonesAndAudit(user, oldPhones, newPhones, updaterUsername);

        // 4️⃣ ID Number
        if (updatedData.getIdNumber() != null && !updatedData.getIdNumber().isBlank()
                && !updatedData.getIdNumber().equals(user.getIdNumber())) {
            if (userRepository.existsByIdNumber(updatedData.getIdNumber()))
                throw new InvalidUserDataException("ID number already in use");
            changes.put("idNumber", new String[]{user.getIdNumber(), updatedData.getIdNumber()});
            user.setIdNumber(updatedData.getIdNumber());
        }

        // 5️⃣ Password
        if (updatedData.getPassword() != null && !updatedData.getPassword().isBlank())
            user.setPassword(passwordEncoder.encode(updatedData.getPassword()));

        // 6️⃣ Role
        if (updatedData.getRole() != null) {
            if (user.getUsername().equals(updaterUsername))
                throw new UnauthorizedAccessException("You cannot change your own role");
            if (hasManagerialRole(updaterUsername)) {
                changes.put("role", new String[]{user.getRole().name(), updatedData.getRole().toUpperCase()});
                user.setRole(Role.valueOf(updatedData.getRole().toUpperCase()));
            } else throw new UnauthorizedAccessException("You are not authorized to change user roles");
        }

        user = userRepository.save(user);
        auditChanges(user, changes, updaterUsername);

        return mapToDTO(user);
    }

    @Transactional
    public ResponseEntity<?> updateUserImages(
            String identifier,
            List<MultipartFile> newFiles,
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
        return ResponseEntity.ok(mapToDTO(target));
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

    public UserDTO getActiveUser(String identifier)  {
        User user = userRepository.findByIdentifier(identifier)
                .orElseThrow(() -> new UserNotFoundException("User not found: " + identifier));
        return mapToDTO(user);
    }

    public UserDTO getActiveOrDeletedUser(String identifier) throws UserNotFoundException {
        User user = userRepository.findByIdentifierForAudits(identifier)
                .orElseThrow(() -> new UserNotFoundException("User not found: " + identifier));
        return mapToDTO(user);
    }

    public List<UserDTO> getAllActiveUsers() {
        List<User> users = userRepository.findByDeletedFalse();
        return users.stream()
                .map(this::mapToDTO)
                .toList();

    }
    public List<UserDTO> getDeletedUsers() {
        List<User> users = userRepository.findByDeletedTrue();
        return users.stream()
                .map(this::mapToDTO)
                .toList();
    }
    public List<UserDTO> getAllUsersIncludingDeleted() {
        List<User> users = userRepository.findAll();
        return  users.stream()
                .map(this::mapToDTO)
                .toList();
    }

    public List<UserDTO> getUsersByRole(Role requestedRole) {
        try {
            Role currentUserRole = privilegesChecker.getCurrentUserRole();
            if (!currentUserRole.canAccess(requestedRole)) {
                throw new UnauthorizedAccessException("You cannot access users with a higher role than yours.");
            }
            List<User> users = userRepository.findActiveUsersByRole(requestedRole);

            List<UserDTO> dtos = users.stream()
                    .map(this::mapToDTO)
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
    public ResponseEntity<ApiResponse> softDeleteUser(UUID id, Authentication authentication) {
        User user = userRepository.findById(id)
                .orElseThrow(() -> new UserNotFoundException("User not found: " + id));

        UUID performedById = privilegesChecker.getAuthenticatedUser(authentication).getId();
        String performedByUsername = privilegesChecker.getAuthenticatedUser(authentication).getUsername();

        Map<String, Object> modified = softDeleteUserInternal(user, performedById, performedByUsername);

        return ResponseEntity.ok(new ApiResponse(
                "success",
                "User soft deleted successfully",
                List.of(modified)
        ));
    }

    @Transactional
    public ResponseEntity<ApiResponse> softDeleteUsersInBulk(List<UUID> userIds, Authentication authentication) {
        UUID performedById = privilegesChecker.getAuthenticatedUser(authentication).getId();
        String performedByUsername = privilegesChecker.getAuthenticatedUser(authentication).getUsername();

        List<Map<String, Object>> modifiedUsers = new ArrayList<>();
        for (UUID id : userIds) {
            User user = userRepository.findById(id)
                    .orElseThrow(() -> new UserNotFoundException("User not found: " + id));
            modifiedUsers.add(softDeleteUserInternal(user, performedById, performedByUsername));
        }

        return ResponseEntity.ok(new ApiResponse(
                "success",
                "Users soft deleted successfully",
                modifiedUsers
        ));
    }

    /** Internal helper for soft delete */
    private Map<String, Object> softDeleteUserInternal(User user, UUID performedById, String performedByUsername) {
        // Audit user
        userAuditRepository.save(UserAudit.builder()
                .userId(user.getId())
                .username(user.getUsername())
                .role(user.getRole() != null ? user.getRole().name() : null)
                .action("SOFT_DELETE")
                .reason("Soft deletion of user")
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
    public ResponseEntity<ApiResponse> restoreUser(UUID id, Authentication authentication) throws IOException {
        User user = userRepository.findById(id)
                .orElseThrow(() -> new UserNotFoundException("User not found: " + id));

        UUID performedById = privilegesChecker.getAuthenticatedUser(authentication).getId();
        String performedByUsername = privilegesChecker.getAuthenticatedUser(authentication).getUsername();

        Map<String, Object> restored = restoreUserInternal(user, performedById, performedByUsername);

        return ResponseEntity.ok(new ApiResponse(
                "success",
                "User restored successfully",
                List.of(restored)
        ));
    }

    @Transactional
    public ResponseEntity<ApiResponse> restoreUsersInBulk(List<UUID> userIds, Authentication authentication) throws IOException {
        UUID performedById = privilegesChecker.getAuthenticatedUser(authentication).getId();
        String performedByUsername = privilegesChecker.getAuthenticatedUser(authentication).getUsername();

        List<Map<String, Object>> restoredUsers = new ArrayList<>();
        for (UUID id : userIds) {
            User user = userRepository.findById(id)
                    .orElseThrow(() -> new UserNotFoundException("User not found: " + id));
            restoredUsers.add(restoreUserInternal(user, performedById, performedByUsername));
        }

        return ResponseEntity.ok(new ApiResponse(
                "success",
                "Users restored successfully",
                restoredUsers
        ));
    }

    /** Internal helper for restore */
    private Map<String, Object> restoreUserInternal(User user, UUID performedById, String performedByUsername) throws IOException {
        // Audit restore
        userAuditRepository.save(UserAudit.builder()
                .userId(user.getId())
                .username(user.getUsername())
                .role(user.getRole() != null ? user.getRole().name() : null)
                .action("RESTORE")
                .reason("Restoring user")
                .performedById(performedById)
                .performedByUsername(performedByUsername)
                .timestamp(LocalDateTime.now())
                .build());

        // Restore user
        user.setDeleted(false);
        userRepository.save(user);

        // Unhide images
        Path userDir = Paths.get(fileStorageProperties.getUserUploadDir(), user.getUploadFolder())
                .toAbsolutePath().normalize();
        if (Files.exists(userDir)) {
            Files.walk(userDir)
                    .filter(Files::isRegularFile)
                    .forEach(path -> {
                        try { userImageService.hidePath(path); } catch (IOException e) { log.warn("Failed to unhide file: {}", path, e); }
                    });
            userImageService.hidePath(userDir);
        }

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
        userImageService.deleteUserUploadDirectory(userDir);

        // Delete the user
        userRepository.delete(user);

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
                                   List<MultipartFile> newFiles,
                                   Authentication auth) throws IOException {

        List<String> uploadedUrls = userImageService.saveFiles(
                newFiles,
                target.getUploadFolder(),
                fileStorageProperties.getUserUploadDir()
        );

        for (String url : uploadedUrls) {
            UserImage img = UserImage.builder()
                    .fileName(Paths.get(url).getFileName().toString())
                    .filePath(url)
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

    private Set<String> normalizeEmails(List<String> emails) {
        return emails != null ? emails.stream()
                .filter(Objects::nonNull)
                .map(String::trim)
                .map(String::toLowerCase)
                .filter(s -> !s.isBlank())
                .collect(Collectors.toCollection(LinkedHashSet::new)) : new LinkedHashSet<>();
    }

    private Set<String> normalizePhones(List<String> phones) {
        return phones != null ? phones.stream()
                .filter(Objects::nonNull)
                .map(String::trim)
                .map(this::normalizePhone)
                .filter(Objects::nonNull)
                .collect(Collectors.toCollection(LinkedHashSet::new)) : new LinkedHashSet<>();
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

    private void uploadUserImages(List<MultipartFile> files, String uploadFolder, User user,
                                  UUID creatorId, String creatorUsername) throws IOException {
        if (files == null || files.isEmpty()) return;

        // Base upload directory on disk
        Path baseDir = Paths.get(fileStorageProperties.getUserUploadDir()).toAbsolutePath().normalize();
        Path userUploadDir = baseDir.resolve(uploadFolder);

        // Track folder for rollback
        transactionalFileManager.track(userUploadDir);

        // Save files via UserImageService (returns secure API paths)
        List<String> apiUrls = userImageService.saveFiles(files, uploadFolder, fileStorageProperties.getUserUploadDir());

        for (String apiUrl : apiUrls) {
            // Extract the filename from API URL
            String fileName = Paths.get(apiUrl).getFileName().toString();

            // Track physical file for rollback
            Path physicalFile = userUploadDir.resolve(fileName);
            transactionalFileManager.track(physicalFile);

            // Persist UserImage in DB with API-friendly URL
            UserImage image = UserImage.builder()
                    .fileName(fileName)
                    .filePath(apiUrl)       // ✅ store API URL, not disk path
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

    private String normalizePhone(String phone) {
        if (phone == null) return null;

        // 1️⃣ Remove spaces and hyphens
        String cleaned = phone.replaceAll("[\\s-]", "");

        // 2️⃣ Convert local formats to international
        if (cleaned.matches("^07\\d{7,8}$")) {
            cleaned = "+254" + cleaned.substring(1);
        } else if (cleaned.matches("^01\\d{7,8}$")) {
            cleaned = "+254" + cleaned.substring(1);
        }

        return cleaned;
    }

    private UserDTO mapToDTO(User user) {
        return UserDTO.builder()
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
                        .filter(img -> !img.getDeleted())          // optional: only return non-deleted images
                        .map(UserImage::getFilePath)              // ✅ API URL, not disk path
                        .toList()
                        : List.of())
                .build();
    }
}