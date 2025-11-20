package com.IntegrityTechnologies.business_manager.modules.user.service;

import com.IntegrityTechnologies.business_manager.common.PrivilegesChecker;
import com.IntegrityTechnologies.business_manager.config.FileStorageProperties;
import com.IntegrityTechnologies.business_manager.exception.*;
import com.IntegrityTechnologies.business_manager.modules.user.model.*;
import com.IntegrityTechnologies.business_manager.modules.user.repository.*;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
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

@Slf4j
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

    public List<String> getAllUsersImages(Boolean deleted, Authentication authentication) {
        User performedBy = privilegesChecker.getAuthenticatedUser(authentication);

        List<UserImage> allImages = userImageRepository.findAll();

        // Filter images by authorization
        List<UserImage> authorizedImages = allImages.stream()
                .filter(image -> privilegesChecker.isAuthorized(performedBy, image.getUser())) // only images of users you can access
                .filter(image -> {
                    if (deleted == null) return true;
                    return Boolean.TRUE.equals(image.getDeleted()) == deleted;
                })
                .toList();

        if (authorizedImages.isEmpty()) {
            throw new ImageNotFoundException("No accessible images found for your account.");
        }

        // Audit retrieval
        userImageAuditRepository.save(UserImageAudit.builder()
                .userId(null)
                .username("all users")
                .fileName("all images")
                .filePath("all images")
                .action("RETRIEVE")
                .performedById(performedBy.getId())
                .performedByUsername(performedBy.getUsername())
                .timestamp(LocalDateTime.now())
                .build());

        return authorizedImages.stream()
                .map(UserImage::getFilePath)
                .toList();
    }

    public ResponseEntity<Resource> downloadAllUsersImages(Boolean deleted, Authentication authentication) throws IOException {
        User performedBy = privilegesChecker.getAuthenticatedUser(authentication);

        List<UserImage> allImages = userImageRepository.findAll();

        // Filter images by authorization + deleted flag
        List<UserImage> filteredImages = allImages.stream()
                .filter(image -> privilegesChecker.isAuthorized(performedBy, image.getUser()))
                .filter(image -> {
                    if (deleted == null) return true;
                    return Boolean.TRUE.equals(image.getDeleted()) == deleted;
                })
                .toList();

        if (filteredImages.isEmpty()) {
            throw new ImageNotFoundException("No accessible images found for your account.");
        }

        // Create temporary ZIP file
        File tempZip = File.createTempFile("all-images-", ".zip");

        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(tempZip))) {
            for (UserImage image : filteredImages) {
                Path filePath = Paths.get(fileStorageProperties.getUserUploadDir(), image.getFileName())
                        .toAbsolutePath().normalize();

                if (!Files.exists(filePath) || !Files.isRegularFile(filePath)) {
                    log.warn("Skipping missing image file: {}", image.getFileName());
                    continue;
                }

                zos.putNextEntry(new ZipEntry(image.getFileName()));
                try (InputStream is = Files.newInputStream(filePath)) {
                    is.transferTo(zos);
                }
                zos.closeEntry();

                // Audit each image retrieval/download
                userImageAuditRepository.save(UserImageAudit.builder()
                        .userId(image.getUser().getId())
                        .username(image.getUser().getUsername())
                        .fileName(image.getFileName())
                        .filePath(image.getFilePath())
                        .action("DOWNLOAD")
                        .performedById(performedBy.getId())
                        .performedByUsername(performedBy.getUsername())
                        .timestamp(LocalDateTime.now())
                        .build());
            }
            zos.finish();
        }

        InputStreamResource resource = new InputStreamResource(new FileInputStream(tempZip));

        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"all-images.zip\"")
                .body(resource);
    }

    public List<String> getAllUserImagesForAUser(String identifier, Authentication authentication, Boolean deleted) {
        User target = validateAccess(identifier, authentication, "view");

        if (target.getImages() == null || target.getImages().isEmpty()) {
            throw new ImageNotFoundException("User has no images: " + target.getUsername());
        }

        List<String> results = target.getImages().stream()
                .filter(image -> {
                    if (deleted == null) return true;                // return all
                    if (deleted) return Boolean.TRUE.equals(image.getDeleted());  // return only deleted
                    return !Boolean.TRUE.equals(image.getDeleted()); // return only active
                })
                .map(UserImage::getFilePath)
                .toList();

        if (results.isEmpty()) {
            throw new ImageNotFoundException(
                    "No images match deleted=" + deleted + " for user: " + target.getUsername()
            );
        }

        return results;
    }

    public ResponseEntity<Resource> downloadAllUserImages(String identifier, Authentication authentication, Boolean deleted) throws IOException {
        User target = validateAccess(identifier, authentication, "download");

        List<UserImage> images = userImageRepository.findByUser(target);

        if (images.isEmpty()) {
            throw new ImageNotFoundException("User has no images: " + target.getUsername());
        }

        List<UserImage> filteredImages = images.stream()
                .filter(image -> {
                    if (deleted == null) return true;                              // return all
                    if (deleted) return Boolean.TRUE.equals(image.getDeleted());   // only deleted
                    return !Boolean.TRUE.equals(image.getDeleted());               // only active
                })
                .toList();

        if (filteredImages.isEmpty()) {
            throw new ImageNotFoundException(
                    "No images match deleted=" + deleted + " for user: " + target.getUsername()
            );
        }

        Path userDir = Paths.get(fileStorageProperties.getUserUploadDir(), target.getUploadFolder()).toAbsolutePath().normalize();

        // Create temp zip file
        File tempZip = File.createTempFile("user-images-", ".zip");

        try {
            try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(tempZip))) {
                for (UserImage image : filteredImages) {
                    Path filePath = userDir.resolve(image.getFileName()).normalize();

                    if (!Files.exists(filePath) || !Files.isRegularFile(filePath)) {
                        throw new ImageNotFoundException(
                                "Image record exists but file missing on disk: " + image.getFileName()
                        );
                    }

                    zos.putNextEntry(new ZipEntry(image.getFileName()));
                    try (InputStream is = Files.newInputStream(filePath)) {
                        is.transferTo(zos);
                    }
                    zos.closeEntry();

                    // Audit retrieval
                    userImageAuditRepository.save(UserImageAudit.builder()
                            .userId(target.getId())
                            .username(target.getUsername())
                            .fileName(image.getFileName())
                            .filePath(image.getFilePath())
                            .action("RETRIEVE")
                            .performedById(privilegesChecker.getAuthenticatedUser(authentication).getId())
                            .performedByUsername(privilegesChecker.getAuthenticatedUser(authentication).getUsername())
                            .timestamp(LocalDateTime.now())
                            .build());
                }
            }

            InputStreamResource resource = new InputStreamResource(new FileInputStream(tempZip));

            return ResponseEntity.ok()
                    .contentType(MediaType.APPLICATION_OCTET_STREAM)
                    .header(HttpHeaders.CONTENT_DISPOSITION,
                            "attachment; filename=\"" + target.getUploadFolder() + "-images.zip\"")
                    .body(resource);
        } finally {
            // Cleanup temp file after response is sent
            if (tempZip.exists()) {
                tempZip.delete();
            }
        }
    }

    public ResponseEntity<Resource> getUserImage(String identifier, String filename, Authentication authentication, Boolean deleted) throws IOException {
        // 1. Validate access for the authenticated user
        User target = validateAccess(identifier, authentication, "access");

        // 2. Lookup the image record in the DB
        UserImage image = userImageRepository.findByUserAndFileName(target, filename)
                .orElseThrow(() ->
                        new ImageNotFoundException("Image not found: " + filename + " for user: " + target.getUsername())
                );

        // 3. Apply deleted flag filter
        if (deleted != null) {
            if (deleted && !Boolean.TRUE.equals(image.getDeleted())) {
                throw new ImageAccessDeniedException("Requested deleted image, but image is active: " + filename);
            }
            if (!deleted && Boolean.TRUE.equals(image.getDeleted())) {
                throw new ImageAccessDeniedException("Requested active image, but image is deleted: " + filename);
            }
        }

        // 4. Normalize path and ensure file exists
        Path userDir = Paths.get(fileStorageProperties.getUserUploadDir(), target.getUploadFolder()).toAbsolutePath().normalize();
        Path filePath = userDir.resolve(image.getFileName()).normalize();

        if (!Files.exists(filePath) || !Files.isRegularFile(filePath)) {
            throw new ImageNotFoundException(
                    "Image entry exists in DB but file missing on disk: " + filePath
            );
        }

        // 5. Determine content type
        String contentType = Files.probeContentType(filePath);
        if (contentType == null) contentType = "application/octet-stream";

        Resource resource = new org.springframework.core.io.FileSystemResource(filePath.toFile());

        // 6. Audit retrieval
        User performedBy = privilegesChecker.getAuthenticatedUser(authentication);
        userImageAuditRepository.save(UserImageAudit.builder()
                .userId(target.getId())
                .username(target.getUsername())
                .fileName(image.getFileName())
                .filePath(image.getFilePath())
                .action("RETRIEVE")
                .performedById(performedBy.getId())
                .performedByUsername(performedBy.getUsername())
                .timestamp(LocalDateTime.now())
                .build());

        // 7. Return the resource
        return ResponseEntity.ok()
                .contentType(MediaType.parseMediaType(contentType))
                .header(HttpHeaders.CONTENT_DISPOSITION, "inline; filename=\"" + image.getFileName() + "\"")
                .body(resource);
    }

    public ResponseEntity<?> softdeleteUserImage(String identifier, String filename, Authentication authentication) throws IOException {
        User target = validateAccess(identifier, authentication, "delete");

        userImageRepository.findByUserAndFileName(target, filename).ifPresent(image -> {
            image.setDeleted(true);
            image.setDeletedAt(LocalDateTime.now());
            userImageRepository.save(image);

            userImageAuditRepository.save(UserImageAudit.builder()
                    .userId(target.getId())
                    .username(target.getUsername())
                    .fileName(image.getFileName())
                    .filePath(image.getFilePath())
                    .action("SOFT_DELETE")
                    .performedById(privilegesChecker.getAuthenticatedUser(authentication).getId())
                    .performedByUsername(privilegesChecker.getAuthenticatedUser(authentication).getUsername())
                    .timestamp(LocalDateTime.now())
                    .build());
        });

        return ResponseEntity.ok("Image deleted successfully: " + filename);
    }

    public ResponseEntity<?> softdeleteAllUserImages(String identifier, Authentication authentication) throws IOException {
        User target = validateAccess(identifier, authentication, "delete");

        List<UserImage> images = userImageRepository.findByUser(target);
        LocalDateTime now = LocalDateTime.now();
        UUID adminId = privilegesChecker.getAuthenticatedUser(authentication).getId();
        String adminUsername = privilegesChecker.getAuthenticatedUser(authentication).getUsername();

        images.forEach(image -> {
            image.setDeleted(true);
            image.setDeletedAt(now);
            userImageRepository.save(image);

            // Audit
            userImageAuditRepository.save(UserImageAudit.builder()
                    .userId(target.getId())
                    .username(target.getUsername())
                    .fileName(image.getFileName())
                    .filePath(image.getFilePath())
                    .action("SOFT_DELETE_ALL")
                    .performedById(adminId)
                    .performedByUsername(adminUsername)
                    .timestamp(now)
                    .build());
        });

        return ResponseEntity.ok("All user images deleted for user: " + target.getUploadFolder());
    }

    public ResponseEntity<?> restoreUserImage(String identifier, String filename, Authentication authentication) throws IOException {
        User target = validateAccess(identifier, authentication, "restore");

        Optional<UserImage> imageOpt = userImageRepository.findByUserAndFileName(target, filename);

        if (imageOpt.isEmpty()) {
            throw new ImageNotFoundException("Image not found: " + filename + " for user: " + identifier);
        }
        if (!Boolean.TRUE.equals(imageOpt.get().getDeleted())) {
            throw new ImageAccessDeniedException("Image " + filename + " not deleted: " + " for user: " + identifier);
        }

        UserImage image = imageOpt.get();

        if(Boolean.TRUE.equals(image.getUser().getDeleted())) {
            throw new ImageAccessDeniedException("You cannot restore the images of the deleted user: " + image.getUser().getUsername());
        }

        // Restore DB flags
        image.setDeleted(false);
        image.setDeletedAt(null);
        userImageRepository.save(image);

        // Restore audit
        userImageAuditRepository.save(UserImageAudit.builder()
                .userId(target.getId())
                .username(target.getUsername())
                .fileName(image.getFileName())
                .filePath(image.getFilePath())
                .action("RESTORE")
                .performedById(privilegesChecker.getAuthenticatedUser(authentication).getId())
                .performedByUsername(privilegesChecker.getAuthenticatedUser(authentication).getUsername())
                .timestamp(LocalDateTime.now())
                .build());

        return ResponseEntity.ok("Image restored successfully: " + filename);
    }

    public ResponseEntity<?> restoreAllUserImages(String identifier, Authentication authentication) throws IOException {
        User target = validateAccess(identifier, authentication, "restore");

        List<UserImage> images = userImageRepository.findByUser(target);
        LocalDateTime now = LocalDateTime.now();

        images.stream()
                .filter(image -> Boolean.TRUE.equals(image.getDeleted()))
                .forEach(image -> {
                    image.setDeleted(false);
                    image.setDeletedAt(null);
                    userImageRepository.save(image);

                    // Audit record
                    userImageAuditRepository.save(UserImageAudit.builder()
                            .userId(target.getId())
                            .username(target.getUsername())
                            .fileName(image.getFileName())
                            .filePath(image.getFilePath())
                            .action("RESTORE_ALL")
                            .performedById(privilegesChecker.getAuthenticatedUser(authentication).getId())
                            .performedByUsername(privilegesChecker.getAuthenticatedUser(authentication).getUsername())
                            .timestamp(now)
                            .build());
                });

        return ResponseEntity.ok("All soft-deleted images restored for user: " + target.getUploadFolder());
    }

    public ResponseEntity<?> harddeleteUserImage(String identifier, String filename, Authentication authentication) throws IOException {
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



    public ResponseEntity<?> harddeleteAllUserImages(String identifier, Authentication authentication) throws IOException {
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