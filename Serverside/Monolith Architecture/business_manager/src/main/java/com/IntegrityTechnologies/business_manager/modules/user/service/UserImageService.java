package com.IntegrityTechnologies.business_manager.modules.user.service;

import com.IntegrityTechnologies.business_manager.common.PrivilegesChecker;
import com.IntegrityTechnologies.business_manager.config.FileStorageProperties;
import com.IntegrityTechnologies.business_manager.exception.*;
import com.IntegrityTechnologies.business_manager.modules.user.model.*;
import com.IntegrityTechnologies.business_manager.modules.user.repository.*;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.InputStreamResource;
import org.springframework.core.io.Resource;
import org.springframework.http.*;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.*;
import java.nio.file.*;
import java.nio.file.attribute.DosFileAttributeView;
import java.time.LocalDateTime;
import java.util.*;
import java.util.Comparator;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

@Service
@RequiredArgsConstructor
public class UserImageService {

    private final UserRepository userRepository;
    private final UserImageRepository userImageRepository;
    private final UserImageAuditRepository userImageAuditRepository;
    private final FileStorageProperties fileStorageProperties;
    private final PrivilegesChecker privilegesChecker;

    /* ====================== VALIDATE ACCESS ====================== */
    private User validateAccess(String identifier, Authentication authentication, String action) {
        User requester = privilegesChecker.getAuthenticatedUser(authentication);
        User target = userRepository.findByIdentifier(identifier)
                .orElseThrow(() -> new UserNotFoundException("User not found: " + identifier));

        if (!privilegesChecker.isAuthorized(requester, target)) {
            throw new UnauthorizedAccessException("You do not have permission to " + action + " this user's images");
        }
        return target;
    }

    /* ====================== SAVE FILES ====================== */
    public List<String> saveFiles(List<MultipartFile> files, String folderName, String baseDir) throws IOException {
        List<String> urls = new ArrayList<>();
        Path userDir = Paths.get(baseDir, folderName).toAbsolutePath().normalize();

        // Create and hide folders
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

            // Use secure API path, not physical path
            urls.add("/api/users/images/" + folderName + "/" + fileName);
        }

        return urls;
    }

    private void createAndHideBaseFolders(String baseDir) throws IOException {
        Path basePath = Paths.get(baseDir).toAbsolutePath().normalize();
        Path uploadsFolder = basePath.getParent();

        if (uploadsFolder != null && !Files.exists(uploadsFolder)) {
            Files.createDirectories(uploadsFolder);
            hidePath(uploadsFolder);
        }

        if (!Files.exists(basePath)) {
            Files.createDirectories(basePath);
        }
        hidePath(basePath);
    }

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

    private void checkDiskSpace(Path path, long fileSize) {
        if (path.toFile().getFreeSpace() < fileSize) throw new StorageFullException("Not enough disk space to save file");
    }

    /* ====================== RETRIEVE / DOWNLOAD / DELETE ====================== */
    public List<String> getAllUserImages(String identifier, Authentication authentication) {
        User target = validateAccess(identifier, authentication, "view");
        return target.getImages() != null
                ? target.getImages().stream()
                .map(userImage -> userImage.getFilePath())
                .toList()
                : List.of();
    }

    public ResponseEntity<Resource> downloadAllUserImages(String identifier, Authentication authentication) throws IOException {
        User target = validateAccess(identifier, authentication, "download");
        Path userDir = Paths.get(fileStorageProperties.getUserUploadDir(), target.getUploadFolder()).toAbsolutePath().normalize();

        if (!Files.exists(userDir) || !Files.isDirectory(userDir)) {
            throw new DirectoryNotFoundException("User", target.getUsername(), userDir.toString());
        }

        File tempZip = File.createTempFile("user-images-", ".zip");
        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(tempZip))) {
            Files.walk(userDir).filter(Files::isRegularFile).forEach(filePath -> {
                ZipEntry entry = new ZipEntry(filePath.getFileName().toString());
                try (InputStream is = Files.newInputStream(filePath)) {
                    zos.putNextEntry(entry);
                    is.transferTo(zos);
                    zos.closeEntry();

                    //  record & audit
                    userImageAuditRepository.save(UserImageAudit.builder()
                        .userId(target.getId())
                        .username(target.getUsername())
                        .fileName(filePath.getFileName().toString())
                        .filePath(filePath.toString())
                        .action("RETRIEVE")
                        .performedById(privilegesChecker.getAuthenticatedUser(authentication).getId())
                        .performedByUsername(privilegesChecker.getAuthenticatedUser(authentication).getUsername())
                        .timestamp(LocalDateTime.now())
                        .build());

                } catch (IOException e) { e.printStackTrace(); }
            });
        }

        InputStreamResource resource = new InputStreamResource(new FileInputStream(tempZip));

        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + target.getUploadFolder() + "-images.zip\"")
                .body(resource);
    }

    public ResponseEntity<Resource> getUserImage(String identifier, String filename, Authentication authentication) throws IOException {
        User target = validateAccess(identifier, authentication, "access");
        Path filePath = Paths.get(fileStorageProperties.getUserUploadDir(), target.getUploadFolder(), filename).toAbsolutePath().normalize();

        if (!Files.exists(filePath) || !Files.isRegularFile(filePath)) {
            throw new DirectoryNotFoundException("User", target.getUsername(), filePath.toString());
        }

        String contentType = Files.probeContentType(filePath);
        if (contentType == null) contentType = "application/octet-stream";
        Resource resource = new org.springframework.core.io.FileSystemResource(filePath.toFile());

        //  record & audit
        userImageAuditRepository.save(UserImageAudit.builder()
            .userId(target.getId())
            .username(target.getUsername())
            .fileName(filePath.getFileName().toString())
            .filePath(filePath.toString())
            .action("RETRIEVE")
            .performedById(privilegesChecker.getAuthenticatedUser(authentication).getId())
            .performedByUsername(privilegesChecker.getAuthenticatedUser(authentication).getUsername())
            .timestamp(LocalDateTime.now())
            .build());

        return ResponseEntity.ok()
                .contentType(MediaType.parseMediaType(contentType))
                .header(HttpHeaders.CONTENT_DISPOSITION, "inline; filename=\"" + filePath.getFileName() + "\"")
                .body(resource);
    }

    public ResponseEntity<?> deleteUserImage(String identifier, String filename, Authentication authentication) throws IOException {
        User target = validateAccess(identifier, authentication, "delete");
        Path filePath = Paths.get(fileStorageProperties.getUserUploadDir(), target.getUploadFolder(), filename).toAbsolutePath().normalize();

        if (!Files.exists(filePath) || !Files.isRegularFile(filePath)) {
            throw new DirectoryNotFoundException("User", target.getUsername(), filePath.toString());
        }

        Files.delete(filePath);

        // If user directory is empty after deletion, remove it
        Path baseDir = Paths.get(fileStorageProperties.getUserUploadDir()).toAbsolutePath().normalize();
        Path userDir = baseDir.resolve(target.getUploadFolder()).normalize();
        try (var files = Files.list(userDir)) {
            if (!files.findAny().isPresent()) {
                Files.deleteIfExists(userDir);
            }
        }


        // Delete record & audit
        userImageRepository.findByUserAndFileName(target, filename).ifPresent(image -> {
            userImageRepository.delete(image);
            userImageAuditRepository.save(UserImageAudit.builder()
                    .userId(target.getId())
                    .username(target.getUsername())
                    .fileName(image.getFileName())
                    .filePath(image.getFilePath())
                    .action("DELETE")
                    .performedById(privilegesChecker.getAuthenticatedUser(authentication).getId())
                    .performedByUsername(privilegesChecker.getAuthenticatedUser(authentication).getUsername())
                    .timestamp(LocalDateTime.now())
                    .build());
        });

        return ResponseEntity.ok("Image deleted successfully: " + filename);
    }

    public ResponseEntity<?> deleteAllUserImages(String identifier, Authentication authentication) throws IOException {
        User target = validateAccess(identifier, authentication, "delete");
        Path userDir = Paths.get(fileStorageProperties.getUserUploadDir(), target.getUploadFolder()).toAbsolutePath().normalize();

        if (!Files.exists(userDir) || !Files.isDirectory(userDir)) {
            throw new DirectoryNotFoundException("User", target.getUsername(), userDir.toString());
        }

        deleteUserUploadDirectory(userDir);

        // Delete DB records & audits
        List<UserImage> images = userImageRepository.findByUser(target);
        images.forEach(image -> {
            userImageAuditRepository.save(UserImageAudit.builder()
                    .userId(target.getId())
                    .username(target.getUsername())
                    .fileName(image.getFileName())
                    .filePath(image.getFilePath())
                    .action("DELETE_ALL")
                    .performedById(privilegesChecker.getAuthenticatedUser(authentication).getId())
                    .performedByUsername(privilegesChecker.getAuthenticatedUser(authentication).getUsername())
                    .timestamp(LocalDateTime.now())
                    .build());
            userImageRepository.delete(image);
        });

        return ResponseEntity.ok("All images and upload directory deleted successfully for user: " + target.getUploadFolder());
    }

    public void deleteUserUploadDirectory(Path userDir) throws IOException {
        if (Files.exists(userDir) && Files.isDirectory(userDir)) {
            Files.walk(userDir).sorted(Comparator.reverseOrder()).forEach(path -> {
                try { Files.deleteIfExists(path); } catch (IOException e) { e.printStackTrace(); }
            });
        }
    }
}