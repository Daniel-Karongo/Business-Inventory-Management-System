package com.IntegrityTechnologies.business_manager.modules.user.service;

import com.IntegrityTechnologies.business_manager.common.PrivilegesChecker;
import com.IntegrityTechnologies.business_manager.config.FileStorageProperties;
import com.IntegrityTechnologies.business_manager.exception.StorageFullException;
import com.IntegrityTechnologies.business_manager.exception.UnauthorizedAccessException;
import com.IntegrityTechnologies.business_manager.exception.UserNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.user.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.InputStreamResource;
import org.springframework.core.io.Resource;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.security.core.Authentication;
import org.springframework.web.multipart.MultipartFile;

import java.io.*;
import java.nio.file.*;
import java.nio.file.attribute.DosFileAttributeView;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

@Service
@RequiredArgsConstructor
public class UserImageService {

    private final UserRepository userRepository;
    private final FileStorageProperties fileStorageProperties;
    private final PrivilegesChecker privilegesChecker;

    private User validateAccess(String identifier, Authentication authentication, String action) {
        User requester = privilegesChecker.getAuthenticatedUser(authentication);
        User target = userRepository.findByIdentifier(identifier)
                .orElseThrow(() -> new UserNotFoundException("User not found: " + identifier));

        if (!privilegesChecker.isAuthorized(requester, target)) {
            throw new UnauthorizedAccessException("You do not have permission to " + action + " this user's images");
        }
        return target;
    }

    /* ====================== SAVE FILES (HIDDEN, CROSS-PLATFORM) ====================== */
    List<String> saveFiles(List<MultipartFile> files, String folderName, String baseDir) throws IOException {
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
    void hidePath(Path path) throws IOException {
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


    /* ====================== IMAGES RETRIEVAL ====================== */

    public List<String> getAllUserImages(String identifier, Authentication authentication) {
        User target = validateAccess(identifier, authentication, "view");
        return target.getIdImageUrls();
    }

    public ResponseEntity<Resource> downloadAllUserImages(String identifier, Authentication authentication) throws IOException {
        User target = validateAccess(identifier, authentication, "download");

        Path baseDir = Paths.get(fileStorageProperties.getUserUploadDir()).toAbsolutePath().normalize();
        Path userDir = baseDir.resolve(target.getUsername()).normalize();

        if (!Files.exists(userDir) || !Files.isDirectory(userDir)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).build();
        }

        File tempZip = File.createTempFile("user-images-", ".zip");
        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(tempZip))) {
            Files.walk(userDir)
                    .filter(Files::isRegularFile)
                    .forEach(filePath -> {
                        ZipEntry entry = new ZipEntry(filePath.getFileName().toString());
                        try (InputStream is = Files.newInputStream(filePath)) {
                            zos.putNextEntry(entry);
                            is.transferTo(zos);
                            zos.closeEntry();
                        } catch (IOException e) {
                            e.printStackTrace();
                        }
                    });
        }

        InputStreamResource resource = new InputStreamResource(new FileInputStream(tempZip));

        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + target.getUsername() + "-images.zip\"")
                .body(resource);
    }

    public ResponseEntity<Resource> getUserImage(String identifier, String filename, Authentication authentication) throws IOException {
        User target = validateAccess(identifier, authentication, "access");

        Path baseDir = Paths.get(fileStorageProperties.getUserUploadDir()).toAbsolutePath().normalize();
        Path filePath = baseDir.resolve(target.getUsername()).resolve(filename).normalize();

        if (!Files.exists(filePath) || !Files.isRegularFile(filePath)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).build();
        }

        String contentType = Files.probeContentType(filePath);
        if (contentType == null) contentType = "application/octet-stream";
        Resource resource = new org.springframework.core.io.FileSystemResource(filePath.toFile());

        return ResponseEntity.ok()
                .contentType(MediaType.parseMediaType(contentType))
                .header(HttpHeaders.CONTENT_DISPOSITION, "inline; filename=\"" + filePath.getFileName() + "\"")
                .body(resource);
    }

    public ResponseEntity<?> deleteUserImage(String identifier, String filename, Authentication authentication) throws IOException {
        User target = validateAccess(identifier, authentication, "delete");

        Path baseDir = Paths.get(fileStorageProperties.getUserUploadDir()).toAbsolutePath().normalize();
        Path filePath = baseDir.resolve(target.getUsername()).resolve(filename).normalize();

        if (!Files.exists(filePath) || !Files.isRegularFile(filePath)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body("Image not found: " + filename);
        }

        Files.delete(filePath);

        // If user directory is empty after deletion, remove it
        Path userDir = baseDir.resolve(target.getUsername()).normalize();
        try (var files = Files.list(userDir)) {
            if (!files.findAny().isPresent()) {
                Files.deleteIfExists(userDir);
            }
        }

        return ResponseEntity.ok("Image deleted successfully: " + filename);
    }

    public ResponseEntity<?> deleteAllUserImages(String identifier, Authentication authentication) throws IOException {
        User target = validateAccess(identifier, authentication, "delete");

        Path baseDir = Paths.get(fileStorageProperties.getUserUploadDir()).toAbsolutePath().normalize();
        Path userDir = baseDir.resolve(target.getUsername()).normalize();

        if (!Files.exists(userDir) || !Files.isDirectory(userDir)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body("No image directory found for user: " + target.getUsername());
        }

        // ✅ Reuse the internal helper method
        deleteUserUploadDirectory(userDir);

        return ResponseEntity.ok("All images and upload directory deleted successfully for user: " + target.getUsername());
    }

    /**
     * ✅ Helper method — safely deletes a user’s uploads directory and all its files.
     * Can be reused in hardDeleteUser or any cleanup task.
     */
    public void deleteUserUploadDirectory(Path userDir) throws IOException {
        if (Files.exists(userDir) && Files.isDirectory(userDir)) {
            Files.walk(userDir)
                    .sorted(Comparator.reverseOrder()) // Delete files before directories
                    .forEach(path -> {
                        try {
                            Files.deleteIfExists(path);
                        } catch (IOException e) {
                            e.printStackTrace();
                        }
                    });
        }
    }

}