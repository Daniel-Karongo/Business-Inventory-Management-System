package com.IntegrityTechnologies.business_manager.modules.user.service;

import com.IntegrityTechnologies.business_manager.common.PrivilegesChecker;
import com.IntegrityTechnologies.business_manager.config.FileStorageProperties;
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

    /* ====================== REGISTER USER ====================== */
    @Transactional
    public UserDTO registerUser(UserDTO dto, String creatorUsername) throws IOException {
        // Null-safety for lists
        List<String> inputEmails = dto.getEmailAddresses() != null ? dto.getEmailAddresses() : List.of();
        List<String> inputPhones = dto.getPhoneNumbers() != null ? dto.getPhoneNumbers() : List.of();

        // 1) Normalize & dedupe emails (trim + lower-case)
        Set<String> normalizedEmails = inputEmails.stream()
                .filter(Objects::nonNull)
                .map(String::trim)
                .map(String::toLowerCase)
                .filter(s -> !s.isBlank())
                .collect(Collectors.toCollection(LinkedHashSet::new)); // keep order

        if (normalizedEmails.isEmpty()) {
            throw new InvalidUserDataException("At least one email address is required");
        }

        // 2) Normalize & dedupe phones
        Set<String> normalizedPhones = inputPhones.stream()
                .filter(Objects::nonNull)
                .map(String::trim)
                .map(this::normalizePhone)
                .filter(Objects::nonNull)
                .collect(Collectors.toCollection(LinkedHashSet::new));

        // optional: you may require at least one phone — adjust as needed
        // if (normalizedPhones.isEmpty()) { throw new InvalidUserDataException("At least one phone required"); }

        // 3) Check uniqueness (Option A: reject if any user in DB has any of these)
        for (String email : normalizedEmails) {
            if (userRepository.existsByEmailAddress(email)) {
                throw new InvalidUserDataException("Email already in use: " + email);
            }
        }
        for (String phone : normalizedPhones) {
            if (userRepository.existsByPhoneNumber(phone)) {
                throw new InvalidUserDataException("Phone number already in use: " + phone);
            }
        }

        // 4) Username / ID validations
        if (userRepository.existsByUsername(dto.getUsername())) {
            throw new InvalidUserDataException("Username already in use");
        }
        if (dto.getIdNumber() != null && userRepository.existsByIdNumber(dto.getIdNumber())) {
            throw new InvalidUserDataException("ID number already in use");
        }

        // 5) Create upload folder
        String uploadFolder = UUID.randomUUID().toString() + "_" + System.currentTimeMillis();

        // 6) Build user entity (store lists as List from Set to keep DB predictable)
        User user = User.builder()
                .username(dto.getUsername())
                .password(passwordEncoder.encode(dto.getPassword()))
                .emailAddresses(new ArrayList<>(normalizedEmails))
                .phoneNumbers(new ArrayList<>(normalizedPhones))
                .idNumber(dto.getIdNumber())
                .role(Role.valueOf(dto.getRole().toUpperCase()))
                .uploadFolder(uploadFolder)
                .build();

        if (creatorUsername != null) {
            userRepository.findByUsername(creatorUsername).ifPresent(user::setCreatedBy);
        }

        // 7) Handle image uploads (unchanged logic)
        if (dto.getIdImageFiles() != null && !dto.getIdImageFiles().isEmpty()) {
            List<String> urls = userImageService.saveFiles(dto.getIdImageFiles(), uploadFolder, fileStorageProperties.getUserUploadDir());
            for (String url : urls) {
                UserImage image = UserImage.builder()
                    .fileName(Paths.get(url).getFileName().toString())
                    .filePath(url)
                    .user(user)
                    .uploadedAt(LocalDateTime.now())
                    .deleted(false)
                    .build();
                user.getImages().add(image);

                // image audit (performedBy may be null if creatorUsername null; guard accordingly)
                userImageAuditRepository.save(UserImageAudit.builder()
                    .userId(user.getId()) // null until user saved
                    .username(user.getUsername())
                    .fileName(image.getFileName())
                    .filePath(image.getFilePath())
                    .action("UPLOAD")
                    .performedById(userRepository.findByUsername(creatorUsername).get().getId())
                    .performedByUsername(creatorUsername)
                    .timestamp(LocalDateTime.now())
                    .build());
            }
        }

        // 8) Save user (cascade saves images)
        user = userRepository.save(user);

        // 9) Audit creation for each email and phone
        UUID performedById = null;
        if (creatorUsername != null && userRepository.findByUsername(creatorUsername).isPresent()) {
            performedById = userRepository.findByUsername(creatorUsername).get().getId();
        }

        userAuditRepository.save(UserAudit.builder()
            .userId(user.getId())
            .username(user.getUsername())
            .fieldChanged(null)
            .oldValue(null)
            .newValue(null)
            .action("CREATE")
            .performedById(performedById)
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

        // Access check: self or managerial
        if (!updaterUsername.equals(user.getUsername()) && !hasManagerialRole(updaterUsername)) {
            throw new AccessDeniedException("You are not authorized to update this user.");
        }

        Map<String, String[]> changes = new HashMap<>();

        // 1) USERNAME
        String oldUsername = user.getUsername();
        if (updatedData.getUsername() != null && !updatedData.getUsername().isBlank()
                && !updatedData.getUsername().equals(oldUsername)) {
            if (userRepository.existsByUsername(updatedData.getUsername())) {
                throw new InvalidUserDataException("Username already in use");
            }
            changes.put("username", new String[]{oldUsername, updatedData.getUsername()});
            user.setUsername(updatedData.getUsername());
        }

        // 2) EMAILS - normalize (lowercase), dedupe and check uniqueness vs other users
        List<String> oldEmails = user.getEmailAddresses() != null
                ? user.getEmailAddresses()
                : List.of();

        Set<String> newEmailSet = (updatedData.getEmailAddresses() != null)
                ? updatedData.getEmailAddresses().stream()
                .filter(Objects::nonNull)
                .map(String::trim)
                .map(String::toLowerCase)
                .filter(s -> !s.isBlank())
                .collect(Collectors.toCollection(LinkedHashSet::new))
                : new LinkedHashSet<>();

        // (If running older JDK: use new LinkedHashSet<>() and addAll)
        if (newEmailSet.isEmpty() && updatedData.getEmailAddresses() != null && !updatedData.getEmailAddresses().isEmpty()) {
            // fallback for environments without LinkedHashSet.of
            newEmailSet = new LinkedHashSet<>();
            for (String e : updatedData.getEmailAddresses()) {
                if (e != null && !e.isBlank()) newEmailSet.add(e.trim().toLowerCase());
            }
        }

        List<String> newEmails = new ArrayList<>(newEmailSet);

        // uniqueness check per email (Option A: any user blocks it) — but allow if same user already owns it
        for (String email : newEmails) {
            if (!oldEmails.contains(email)) {
                Optional<User> owner = userRepository.findByEmailElementIgnoreCase(email);
                if (owner.isPresent() && !owner.get().getId().equals(user.getId())) {
                    throw new InvalidUserDataException("Email already in use: " + email);
                }
            }
        }

        List<String> addedEmails = newEmails.stream().filter(e -> !oldEmails.contains(e)).toList();
        List<String> removedEmails = oldEmails.stream().filter(e -> !newEmails.contains(e)).toList();

        if (!addedEmails.isEmpty() || !removedEmails.isEmpty()) {
            user.setEmailAddresses(newEmails);
        }

        // 3) PHONES - normalize + dedupe + check uniqueness vs other users
        List<String> oldPhones = user.getPhoneNumbers() != null ? user.getPhoneNumbers() : List.of();

        Set<String> newPhoneSet = (updatedData.getPhoneNumbers() != null)
                ? updatedData.getPhoneNumbers().stream()
                .filter(Objects::nonNull)
                .map(String::trim)
                .map(this::normalizePhone)
                .filter(Objects::nonNull)
                .collect(Collectors.toCollection(LinkedHashSet::new))
                : new LinkedHashSet<>();

        // fallback if needed (same as emails)
        List<String> newPhones = new ArrayList<>(newPhoneSet);

        for (String phone : newPhones) {
            if (!oldPhones.contains(phone)) {
                Optional<User> owner = userRepository.findByPhoneNumberElement(phone);
                if (owner.isPresent() && !owner.get().getId().equals(user.getId())) {
                    throw new InvalidUserDataException("Phone number already in use: " + phone);
                }
            }
        }

        List<String> addedPhones = newPhones.stream().filter(p -> !oldPhones.contains(p)).toList();
        List<String> removedPhones = oldPhones.stream().filter(p -> !newPhones.contains(p)).toList();

        if (!addedPhones.isEmpty() || !removedPhones.isEmpty()) {
            user.setPhoneNumbers(newPhones);
        }

        // 4) ID number
        if (updatedData.getIdNumber() != null && !updatedData.getIdNumber().isBlank()
                && !updatedData.getIdNumber().equals(user.getIdNumber())) {
            if (userRepository.existsByIdNumber(updatedData.getIdNumber())) {
                throw new InvalidUserDataException("ID number already in use");
            }
            changes.put("idNumber", new String[]{user.getIdNumber(), updatedData.getIdNumber()});
            user.setIdNumber(updatedData.getIdNumber());
        }

        // 5) PASSWORD
        if (updatedData.getPassword() != null && !updatedData.getPassword().isBlank()) {
            user.setPassword(passwordEncoder.encode(updatedData.getPassword()));
        }

        // 6) ROLE (only managerial can change)
        if (updatedData.getRole() != null) {
            if (user.getUsername().equals(updaterUsername)) {
                throw new UnauthorizedAccessException("You cannot change your own role");
            }
            if (hasManagerialRole(updaterUsername)) {
                changes.put("role", new String[]{user.getRole().name(), updatedData.getRole().toUpperCase()});
                user.setRole(Role.valueOf(updatedData.getRole().toUpperCase()));
            } else {
                throw new UnauthorizedAccessException("You are not authorized to change user roles");
            }
        }

        // 7) Save
        user = userRepository.save(user);

        // 8) Audit changes
        UUID updaterId = userRepository.findByUsername(updaterUsername).map(User::getId).orElse(null);

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

        for (String added : addedEmails) {
            userAuditRepository.save(UserAudit.builder()
                    .userId(user.getId())
                    .username(user.getUsername())
                    .fieldChanged("email_add")
                    .oldValue(null)
                    .newValue(added)
                    .action("UPDATE")
                    .performedById(updaterId)
                    .performedByUsername(updaterUsername)
                    .timestamp(LocalDateTime.now())
                    .build());
        }
        for (String removed : removedEmails) {
            userAuditRepository.save(UserAudit.builder()
                    .userId(user.getId())
                    .username(user.getUsername())
                    .fieldChanged("email_remove")
                    .oldValue(removed)
                    .newValue(null)
                    .action("UPDATE")
                    .performedById(updaterId)
                    .performedByUsername(updaterUsername)
                    .timestamp(LocalDateTime.now())
                    .build());
        }

        for (String added : addedPhones) {
            userAuditRepository.save(UserAudit.builder()
                    .userId(user.getId())
                    .username(user.getUsername())
                    .fieldChanged("phone_add")
                    .oldValue(null)
                    .newValue(added)
                    .action("UPDATE")
                    .performedById(updaterId)
                    .performedByUsername(updaterUsername)
                    .timestamp(LocalDateTime.now())
                    .build());
        }
        for (String removed : removedPhones) {
            userAuditRepository.save(UserAudit.builder()
                    .userId(user.getId())
                    .username(user.getUsername())
                    .fieldChanged("phone_remove")
                    .oldValue(removed)
                    .newValue(null)
                    .action("UPDATE")
                    .performedById(updaterId)
                    .performedByUsername(updaterUsername)
                    .timestamp(LocalDateTime.now())
                    .build());
        }

        return mapToDTO(user);
    }

    /* ====================== USER IMAGE UPDATE ====================== */
    @Transactional
    public ResponseEntity<?> updateUserImages(String identifier, List<MultipartFile> newFiles, Authentication auth) throws IOException {
        User target = validateAccess(identifier, auth, "update images");
        Path userDir = Paths.get(fileStorageProperties.getUserUploadDir(), target.getUploadFolder()).toAbsolutePath().normalize();

        // Delete old images and records
        userImageService.deleteUserUploadDirectory(userDir);
        userImageRepository.deleteAll(userImageRepository.findByUser(target));
        userImageAuditRepository.save(UserImageAudit.builder()
            .userId(target.getId())
            .username(target.getUsername())
            .fileName("all user's images")
            .filePath(userDir.toString())
            .action("DELETE_ALL")
            .performedById(privilegesChecker.getAuthenticatedUser(auth).getId())
            .performedByUsername(privilegesChecker.getAuthenticatedUser(auth).getUsername())
            .timestamp(LocalDateTime.now())
            .build());



        if (newFiles != null && !newFiles.isEmpty()) {
            List<String> newUrls = userImageService.saveFiles(newFiles, target.getUploadFolder(), fileStorageProperties.getUserUploadDir());

            for (String url : newUrls) {
                UserImage image = UserImage.builder()
                        .fileName(Paths.get(url).getFileName().toString())
                        .filePath(Paths.get(fileStorageProperties.getUserUploadDir(), target.getUsername(), Paths.get(url).getFileName().toString()).toString())
                        .user(target)
                        .uploadedAt(LocalDateTime.now())
                        .deleted(false)
                        .build();
                userImageRepository.save(image);

                userImageAuditRepository.save(UserImageAudit.builder()
                        .userId(target.getId())
                        .username(target.getUsername())
                        .fileName(image.getFileName())
                        .filePath(image.getFilePath())
                        .action("UPLOAD")
                        .performedById(privilegesChecker.getAuthenticatedUser(auth).getId())
                        .performedByUsername(privilegesChecker.getAuthenticatedUser(auth).getUsername())
                        .timestamp(LocalDateTime.now())
                        .build());
            }
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
                .map(user -> user.getRole() == Role.ADMIN || user.getRole() == Role.MANAGER)
                .orElse(false);
    }

    /* ====================== GET USERS ====================== */
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
        return userAuditRepository.findByUserId(userId);
    }
    public List<UserAudit> getUserAuditsPerpetrated(UUID userId) {
        return userAuditRepository.findByPerformedById(userId);
    }

    public List<UserImageAudit> getUserImageAuditsTarget(UUID userId) {
        return userImageAuditRepository.findByUserId(userId);
    }
    public List<UserImageAudit> getUserImageAuditsPerpetrated(UUID userId) {
        return userImageAuditRepository.findByPerformedById(userId);
    }


    /* ====================== SOFT DELETE / RESTORE ====================== */
    @Transactional
    public ResponseEntity<?> softDeleteUser(UUID id, Authentication authentication) {
        User user = userRepository.findById(id)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        // Get currently authenticated user for audit
        UUID performedById = privilegesChecker.getAuthenticatedUser(authentication).getId();
        String performedByUsername = privilegesChecker.getAuthenticatedUser(authentication).getUsername();

        // ----------------------
        // 1️⃣ Audit user record
        // ----------------------
        userAuditRepository.save(UserAudit.builder()
                .userId(user.getId())
                .username(user.getUsername())
                .emailAddress(user.getEmailAddresses().get(0))
                .idNumber(user.getIdNumber())
                .role(user.getRole() != null ? user.getRole().name() : null)
                .action("SOFT_DELETE")
                .reason("Soft deletion of user")
                .performedById(performedById)
                .performedByUsername(performedByUsername)
                .timestamp(LocalDateTime.now())
                .build());

        // ----------------------
        // 2️⃣ Soft delete images & audit each
        // ----------------------
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

        // ----------------------
        // 3️⃣ Mark user as deleted and save
        // ----------------------
        user.setDeleted(true);
        userRepository.save(user);

        return ResponseEntity.ok(new ApiResponse("success", "User deleted successfully"));
    }

    @Transactional
    public ResponseEntity<?> restoreUser(UUID id, Authentication authentication) throws IOException {
        User user = userRepository.findById(id)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        // Get the current user for audit
        UUID performedById = privilegesChecker.getAuthenticatedUser(authentication).getId();
        String performedByUsername = privilegesChecker.getAuthenticatedUser(authentication).getUsername();

        // ----------------------
        // 1️⃣ Audit the restore action
        // ----------------------
        userAuditRepository.save(UserAudit.builder()
                .userId(user.getId())
                .username(user.getUsername())
                .emailAddress(user.getEmailAddresses().get(0))
                .idNumber(user.getIdNumber())
                .role(user.getRole() != null ? user.getRole().name() : null)
                .action("RESTORE")
                .reason("Restoring a previously soft-deleted user")
                .performedById(performedById)
                .performedByUsername(performedByUsername)
                .timestamp(LocalDateTime.now())
                .build());

        // ----------------------
        // 2️⃣ Restore the user
        // ----------------------
        user.setDeleted(false);
        userRepository.save(user);

        // ----------------------
        // 3️⃣ Unhide images on disk
        // ----------------------
        Path userDir = Paths.get(fileStorageProperties.getUserUploadDir(), user.getUploadFolder()).toAbsolutePath().normalize();

        if (Files.exists(userDir)) {
            Files.walk(userDir)
                    .filter(Files::isRegularFile)
                    .forEach(path -> {
                        try { userImageService.hidePath(path); } catch (IOException e) { e.printStackTrace(); }
                    });
            userImageService.hidePath(userDir);
        }

        // ----------------------
        // 4️⃣ Restore images in DB & audit each
        // ----------------------
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

        return ResponseEntity.ok(new ApiResponse("success", "User restored successfully"));
    }


    @Transactional
    public ResponseEntity<?> hardDeleteUser(UUID id, Authentication authentication) throws IOException {
        User user = userRepository.findById(id)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        // Get current user for audit
        UUID performedById = privilegesChecker.getAuthenticatedUser(authentication).getId();
        String performedByUsername = privilegesChecker.getAuthenticatedUser(authentication).getUsername();

        // ----------------------
        // 1️⃣ Audit the user hard delete
        // ----------------------
        userAuditRepository.save(UserAudit.builder()
                .userId(user.getId())
                .username(user.getUsername())
                .emailAddress(user.getEmailAddresses().get(0))
                .idNumber(user.getIdNumber())
                .role(user.getRole() != null ? user.getRole().name() : null)
                .action("HARD_DELETE")
                .reason("Permanent deletion of user")
                .performedById(performedById)
                .performedByUsername(performedByUsername)
                .timestamp(LocalDateTime.now())
                .build());

        // ----------------------
        // 2️⃣ Audit & delete each image
        // ----------------------
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

        // ----------------------
        // 3️⃣ Delete files from disk
        // ----------------------
        Path userDir = Paths.get(fileStorageProperties.getUserUploadDir(), user.getUploadFolder()).toAbsolutePath().normalize();
        userImageService.deleteUserUploadDirectory(userDir);

        // ----------------------
        // 4️⃣ Delete the user
        // ----------------------
        userRepository.delete(user);

        return ResponseEntity.ok(new ApiResponse("success", "User deleted successfully"));
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
                .emailAddresses(user.getEmailAddresses())       // ✅ now a list
                .phoneNumbers(user.getPhoneNumbers())           // ✅ now a list
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
                        .map(UserImage::getFilePath)
                        .toList()
                        : List.of())
                .build();
    }
}