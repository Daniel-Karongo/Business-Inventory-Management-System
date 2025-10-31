package com.IntegrityTechnologies.business_manager.modules.user.service;

import com.IntegrityTechnologies.business_manager.config.FileStorageProperties;
import com.IntegrityTechnologies.business_manager.exception.InvalidUserDataException;
import com.IntegrityTechnologies.business_manager.exception.StorageFullException;
import com.IntegrityTechnologies.business_manager.exception.UserNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.user.dto.UserDTO;
import com.IntegrityTechnologies.business_manager.modules.user.model.Role;
import com.IntegrityTechnologies.business_manager.modules.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.user.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.DosFileAttributeView;
import java.util.*;

@Service
@RequiredArgsConstructor
public class UserService {

    private final UserRepository userRepository;
    private final PasswordEncoder passwordEncoder;
    private final FileStorageProperties fileStorageProperties;

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
            user.setIdImageUrls(saveFiles(dto.getIdImageFiles(), user.getUsername(), fileStorageProperties.getUserUploadDir()));
        }

        // ✅ Save entity
        user = userRepository.save(user);

        // ✅ Convert to DTO for response
        return UserDTO.builder()
                .id(user.getId())
                .username(user.getUsername())
                .emailAddress(user.getEmailAddress())
                .idNumber(user.getIdNumber())
                .role(user.getRole() != null ? user.getRole().name() : null)
                .createdBy(user.getCreatedBy() != null ? user.getCreatedBy().getId() + " | " + user.getCreatedBy().getUsername() : null)
                .deleted(user.isDeleted())
                .idImageUrls(user.getIdImageUrls())
                .build();
    }

    public UserDTO updateUser(String identifier, UserDTO updatedData, String updaterUsername) throws IOException {
        User user = userRepository.findByIdentifier(identifier)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        // only update fields that are present
        if (updatedData.getUsername() != null && !updatedData.getUsername().equals(user.getUsername())) {
            if (userRepository.existsByUsername(updatedData.getUsername())) {
                throw new InvalidUserDataException("Username already in use");
            }
            user.setUsername(updatedData.getUsername());
        }

        if (updatedData.getEmailAddress() != null && !updatedData.getEmailAddress().equals(user.getEmailAddress())) {
            if (userRepository.existsByEmailAddress(updatedData.getEmailAddress())) {
                throw new InvalidUserDataException("Email already in use");
            }
            user.setEmailAddress(updatedData.getEmailAddress());
        }

        if (updatedData.getIdNumber() != null && !updatedData.getIdNumber().equals(user.getIdNumber())) {
            if (userRepository.existsByIdNumber(updatedData.getIdNumber())) {
                throw new InvalidUserDataException("ID number already in use");
            }
            user.setIdNumber(updatedData.getIdNumber());
        }

        if (updatedData.getPassword() != null) {
            user.setPassword(passwordEncoder.encode(updatedData.getPassword()));
        }

        if (updatedData.getRole() != null) {
            user.setRole(Role.valueOf(updatedData.getRole().toUpperCase()));
        }

        // optionally update ID images
        if (updatedData.getIdImageFiles() != null && !updatedData.getIdImageFiles().isEmpty()) {
            user.setIdImageUrls(saveFiles(updatedData.getIdImageFiles(), user.getUsername(), fileStorageProperties.getUserUploadDir()));
        }

        userRepository.save(user);
        return UserDTO.builder()
                .id(user.getId())
                .username(user.getUsername())
                .emailAddress(user.getEmailAddress())
                .idNumber(user.getIdNumber())
                .role(user.getRole() != null ? user.getRole().name() : null)
                .createdBy(user.getCreatedBy() != null ? user.getCreatedBy().getId() + " | " + user.getCreatedBy().getUsername() : null)
                .deleted(user.isDeleted())
                .idImageUrls(user.getIdImageUrls())
                .build();
    }


    /* ====================== SAVE FILES (HIDDEN, CROSS-PLATFORM) ====================== */
    private List<String> saveFiles(List<MultipartFile> files, String folderName, String baseDir) throws IOException {
        List<String> urls = new ArrayList<>();
        Path userDir = Paths.get(baseDir, folderName).toAbsolutePath().normalize();

        // Ensure all base folders exist and are hidden
        createAndHideBaseFolders(baseDir);

        if (!Files.exists(userDir)) Files.createDirectories(userDir);
        hidePath(userDir);

        for (MultipartFile file : files) {
            if (file.isEmpty()) continue;

            checkDiskSpace(userDir, file.getSize());

            String fileName = System.currentTimeMillis() + "_" + file.getOriginalFilename();
            Path filePath = userDir.resolve(fileName);
            file.transferTo(filePath.toFile());
            hidePath(filePath);

            urls.add("/api/users/images/" + folderName + "/" + fileName);
        }

        return urls;
    }

    /* ====================== CREATE & HIDE BASE FOLDERS ====================== */
    /* ====================== CREATE & HIDE BASE FOLDERS ====================== */
    private void createAndHideBaseFolders(String baseDir) throws IOException {
        Path basePath = Paths.get(baseDir).toAbsolutePath().normalize(); // .../uploads/users
        Path uploadsFolder = basePath.getParent();                       // .../uploads

        // 1. Ensure "uploads" exists first and hide it
        if (uploadsFolder != null) {
            if (!Files.exists(uploadsFolder)) {
                Files.createDirectories(uploadsFolder);
            }
            hidePath(uploadsFolder);
        }

        // 2. Ensure current base folder (users/products) exists and hide it
        if (!Files.exists(basePath)) {
            Files.createDirectories(basePath);
        }
        hidePath(basePath);
    }

    /* ====================== HIDE FILE OR FOLDER ====================== */
    private void hidePath(Path path) throws IOException {
        if (System.getProperty("os.name").toLowerCase().contains("win")) {
            DosFileAttributeView attr = Files.getFileAttributeView(path, DosFileAttributeView.class);
            if (attr != null) attr.setHidden(true);
        } else {
            Path parent = path.getParent();
            String name = path.getFileName().toString();
            if (!name.startsWith(".")) {
                Path hiddenPath = parent.resolve("." + name);
                if (!Files.exists(hiddenPath)) Files.move(path, hiddenPath, StandardCopyOption.REPLACE_EXISTING);
            }
        }
    }

    /* ====================== CHECK DISK SPACE ====================== */
    private void checkDiskSpace(Path path, long fileSize) {
        if (path.toFile().getFreeSpace() < fileSize) throw new StorageFullException("Not enough disk space to save file");
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
                        try { hidePath(path); } catch (IOException e) { e.printStackTrace(); }
                    });
            hidePath(userDir);
        }
    }

    /* ====================== HARD DELETE ====================== */
    public void hardDeleteUser(UUID id) throws IOException {
        User user = userRepository.findById(id).orElseThrow(() -> new UserNotFoundException("User not found"));
        Path userDir = Paths.get(fileStorageProperties.getUserUploadDir(), user.getUsername()).toAbsolutePath().normalize();

        if (Files.exists(userDir)) {
            Files.walk(userDir)
                    .sorted(Comparator.reverseOrder())
                    .forEach(path -> {
                        try { Files.deleteIfExists(path); } catch (IOException e) { e.printStackTrace(); }
                    });
        }
        userRepository.delete(user);
    }
}