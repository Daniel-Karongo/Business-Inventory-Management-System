package com.IntegrityTechnologies.business_manager.modules.user.service;

import com.IntegrityTechnologies.business_manager.common.PrivilegesChecker;
import com.IntegrityTechnologies.business_manager.config.FileStorageProperties;
import com.IntegrityTechnologies.business_manager.exception.InvalidUserDataException;
import com.IntegrityTechnologies.business_manager.exception.StorageFullException;
import com.IntegrityTechnologies.business_manager.exception.UnauthorizedAccessException;
import com.IntegrityTechnologies.business_manager.exception.UserNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.user.dto.UserDTO;
import com.IntegrityTechnologies.business_manager.modules.user.model.Role;
import com.IntegrityTechnologies.business_manager.modules.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.user.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.DosFileAttributeView;
import java.time.LocalDateTime;
import java.util.*;

@Service
@RequiredArgsConstructor
public class UserService {

    private final UserRepository userRepository;
    private final PasswordEncoder passwordEncoder;
    private final FileStorageProperties fileStorageProperties;
    private final UserImageService userImageService;
    private final PrivilegesChecker privilegesChecker;

    /* ====================== REGISTER USER ====================== */
    public UserDTO registerUser(UserDTO dto, String creatorUsername) throws IOException {
        if (userRepository.existsByEmailAddress(dto.getEmailAddress())) {
            throw new InvalidUserDataException("Email already in use");
        }
        if (userRepository.existsByUsername(dto.getUsername())) {
            throw new InvalidUserDataException("Username already in use");
        }
        if (dto.getIdNumber() != null && userRepository.existsByIdNumber(dto.getIdNumber())) {
            throw new InvalidUserDataException("ID number already in use");
        }

        User user = User.builder()
                .username(dto.getUsername())
                .password(passwordEncoder.encode(dto.getPassword()))
                .emailAddress(dto.getEmailAddress())
                .idNumber(dto.getIdNumber())
                .role(Role.valueOf(dto.getRole().toUpperCase()))
                .build();

        // ✅ Set creator if exists
        if (creatorUsername != null) {
            userRepository.findByUsername(creatorUsername)
                    .ifPresent(user::setCreatedBy);
        }

        // ✅ Handle file uploads
        if (dto.getIdImageFiles() != null && !dto.getIdImageFiles().isEmpty()) {
            user.setIdImageUrls(userImageService.saveFiles(dto.getIdImageFiles(), user.getUsername(), fileStorageProperties.getUserUploadDir()));
        }

        // ✅ Save entity
        user = userRepository.save(user);

        // ✅ Convert to DTO for response
        return mapToDTO(user);
    }

    // ✅ Update user details (partial update)
    public UserDTO updateUser(String identifier, UserDTO updatedData, String updaterUsername) throws IOException {
        User user = userRepository.findByIdentifier(identifier)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        // 🧩 Authorization
        if (!updaterUsername.equals(user.getUsername()) && !hasManagerialRole(updaterUsername)) {
            throw new AccessDeniedException("You are not authorized to update this user.");
        }

        String oldUsername = user.getUsername();

        /* ----------------------------
         * 🧩 1. USERNAME UPDATE
         * ---------------------------- */
        if (updatedData.getUsername() != null && !updatedData.getUsername().isBlank()
                && !updatedData.getUsername().equals(oldUsername)) {

            if (userRepository.existsByUsername(updatedData.getUsername())) {
                throw new InvalidUserDataException("Username already in use");
            }

            Path baseDir = Paths.get(fileStorageProperties.getUserUploadDir()).toAbsolutePath().normalize();
            Path oldDir = baseDir.resolve(oldUsername);
            Path newDir = baseDir.resolve(updatedData.getUsername());

            if (Files.exists(oldDir)) {
                Files.move(oldDir, newDir, StandardCopyOption.REPLACE_EXISTING);
            }

            user.setUsername(updatedData.getUsername());
        }

        /* ----------------------------
         * 🧩 2. EMAIL UPDATE
         * ---------------------------- */
        if (updatedData.getEmailAddress() != null && !updatedData.getEmailAddress().isBlank()
                && !updatedData.getEmailAddress().equals(user.getEmailAddress())) {

            if (userRepository.existsByEmailAddress(updatedData.getEmailAddress())) {
                throw new InvalidUserDataException("Email already in use");
            }

            user.setEmailAddress(updatedData.getEmailAddress());
        }

        /* ----------------------------
         * 🧩 3. ID NUMBER UPDATE
         * ---------------------------- */
        if (updatedData.getIdNumber() != null && !updatedData.getIdNumber().isBlank()
                && !updatedData.getIdNumber().equals(user.getIdNumber())) {

            if (userRepository.existsByIdNumber(updatedData.getIdNumber())) {
                throw new InvalidUserDataException("ID number already in use");
            }

            user.setIdNumber(updatedData.getIdNumber());
        }

        /* ----------------------------
         * 🧩 4. PASSWORD UPDATE
         * ----------------------------
         * - If null → ignore
         * - If empty string → clear (encode "")
         * ---------------------------- */
        if (updatedData.getPassword() != null) {
            user.setPassword(passwordEncoder.encode(updatedData.getPassword()));
        }

        /* ----------------------------
         * 🧩 5. ROLE UPDATE (Admins/Managers only)
         * ---------------------------- */
        if (updatedData.getRole() != null) {
            if(hasManagerialRole(updaterUsername)) {
                user.setRole(Role.valueOf(updatedData.getRole().toUpperCase()));
            } else {
                throw new UnauthorizedAccessException("You are not authorized to change user roles");
            }
        }

        /* ----------------------------
         * 🧩 6. AUDIT METADATA
         * ---------------------------- */
        User updater = userRepository.findByUsername(updaterUsername)
                .orElseThrow(() -> new UserNotFoundException("Updater not found"));

        user.setLastModifiedBy(updater);
        user.setLastModifiedAt(LocalDateTime.now());

        userRepository.save(user);
        return mapToDTO(user);
    }

    /* ==================================================================================
     * ✅ updateUserImages – manage image update logic separately
     * ================================================================================== */
    public ResponseEntity<?> updateUserImages(String identifier, List<MultipartFile> newFiles, Authentication auth)
            throws IOException {

        User target = validateAccess(identifier, auth, "update images");

        Path userDir = Paths.get(fileStorageProperties.getUserUploadDir(), target.getUsername())
                .toAbsolutePath().normalize();

        // 🧩 If no new files provided → just delete old ones
        if (newFiles == null || newFiles.isEmpty()) {
            userImageService.deleteUserUploadDirectory(userDir);
            target.setIdImageUrls(Collections.emptyList());
            userRepository.save(target);
            return ResponseEntity.ok("All images deleted for user: " + target.getUsername());
        }

        // 🧩 Delete old directory first
        userImageService.deleteUserUploadDirectory(userDir);

        // 🧩 Save new files
        List<String> newImageUrls = userImageService.saveFiles(
                newFiles,
                target.getUsername(),
                fileStorageProperties.getUserUploadDir()
        );

        target.setIdImageUrls(newImageUrls);
        target.setLastModifiedAt(LocalDateTime.now());
        target.setLastModifiedBy(privilegesChecker.getAuthenticatedUser(auth));

        userRepository.save(target);
        return ResponseEntity.ok(mapToDTO(target));
    }

    // 🧩 Access validation helper
    private User validateAccess(String identifier, Authentication auth, String action) {
        User target = userRepository.findByIdentifier(identifier)
                .orElseThrow(() -> new UserNotFoundException("User not found for: " + identifier));

        User requester = privilegesChecker.getAuthenticatedUser(auth);

        if (!privilegesChecker.isAuthorized(requester, target)) {
            throw new UnauthorizedAccessException(
                    "You are not authorized to " + action + " for user: " + target.getUsername()
            );
        }

        return target;
    }

    /* ----------------------------
     * Utility: Check managerial roles
     * ---------------------------- */
    private boolean hasManagerialRole(String username) {
        return userRepository.findByUsername(username)
                .map(user -> user.getRole() == Role.ADMIN || user.getRole() == Role.MANAGER)
                .orElse(false);
    }


    /* ====================== USER RETRIEVAL ====================== */
    public List<User> getAllUsers() { return userRepository.findByDeletedFalse(); }
    public List<User> getDeletedUsers() { return userRepository.findByDeletedTrue(); }
    public List<User> getAllUsersIncludingDeleted() { return userRepository.findAll(); }
    public List<User> getUsersByRole(Role role) { return userRepository.findActiveUsersByRole(role); }

    public User getUserByIdentifier(String identifier) {
        return userRepository.findByIdentifier(identifier).orElseThrow(() -> new UserNotFoundException("User not found"));
    }

    public List<String> getUserImages(String identifier) {
        User user = getUserByIdentifier(identifier);
        return user.getIdImageUrls();
    }

    /* ====================== SOFT DELETE / RESTORE ====================== */
    public void softDeleteUser(UUID id) {
        User user = userRepository.findById(id).orElseThrow(() -> new UserNotFoundException("User not found"));
        user.setDeleted(true);
        userRepository.save(user);
    }

    public void restoreUser(UUID id) throws IOException {
        User user = userRepository.findById(id).orElseThrow(() -> new UserNotFoundException("User not found"));
        user.setDeleted(false);
        userRepository.save(user);

        Path userDir = Paths.get(fileStorageProperties.getUserUploadDir(), user.getUsername()).toAbsolutePath().normalize();
        if (Files.exists(userDir)) {
            Files.walk(userDir)
                    .filter(Files::isRegularFile)
                    .forEach(path -> {
                        try { userImageService.hidePath(path); } catch (IOException e) { e.printStackTrace(); }
                    });
            userImageService.hidePath(userDir);
        }
    }

    /* ====================== HARD DELETE ====================== */
    public void hardDeleteUser(UUID id) throws IOException {
        User user = userRepository.findById(id)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        Path userDir = Paths.get(fileStorageProperties.getUserUploadDir(), user.getUsername())
                .toAbsolutePath().normalize();

        // ✅ Reuse the service helper method
        userImageService.deleteUserUploadDirectory(userDir);

        userRepository.delete(user);
    }

    private UserDTO mapToDTO(User user) {
        return UserDTO.builder()
                .id(user.getId())
                .username(user.getUsername())
                .emailAddress(user.getEmailAddress())
                .idNumber(user.getIdNumber())
                .role(user.getRole() != null ? user.getRole().name() : null)
                .createdBy(user.getCreatedBy() != null
                        ? user.getCreatedBy().getId() + " | " + user.getCreatedBy().getUsername()
                        : null)
                .lastModifiedBy(user.getLastModifiedBy() != null
                        ? user.getLastModifiedBy().getId() + " | " + user.getLastModifiedBy().getUsername()
                        : null)
                .createdAt(user.getCreatedAt())
                .lastModifiedAt(user.getLastModifiedAt())
                .deleted(user.isDeleted())
                .idImageUrls(user.getIdImageUrls())
                .build();
    }

}