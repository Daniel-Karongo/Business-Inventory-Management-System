package com.IntegrityTechnologies.business_manager.modules.person.user.service;

import com.IntegrityTechnologies.business_manager.config.caffeine.CacheInvalidationService;
import com.IntegrityTechnologies.business_manager.config.files.FIleUploadDTO;
import com.IntegrityTechnologies.business_manager.modules.person.user.repository.UserImageAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.user.repository.UserImageRepository;
import com.IntegrityTechnologies.business_manager.modules.person.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.security.util.PrivilegesChecker;
import com.IntegrityTechnologies.business_manager.config.files.FileStorageService;
import com.IntegrityTechnologies.business_manager.config.files.TransactionalFileManager;
import com.IntegrityTechnologies.business_manager.exception.*;
import com.IntegrityTechnologies.business_manager.modules.person.user.dto.UserImageDTO;
import com.IntegrityTechnologies.business_manager.modules.person.user.mapper.UserImageMapper;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.UserImage;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.UserImageAudit;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.io.InputStreamResource;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;
import org.springframework.web.multipart.MultipartFile;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

@Slf4j
@Service
@RequiredArgsConstructor
public class UserImageService {

    private final UserRepository userRepository;
    private final UserImageRepository userImageRepository;
    private final UserImageAuditRepository userImageAuditRepository;
    private final FileStorageService fileStorageService;
    private final PrivilegesChecker privilegesChecker;
    private final TransactionalFileManager transactionalFileManager;
    private final CacheInvalidationService cacheInvalidationService;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    /* ====================== VALIDATE ACCESS ====================== */
    private User validateAccess(String identifier, Authentication authentication, String action) {
        User requester = privilegesChecker.getAuthenticatedUser(authentication);
        User target = userRepository.findByIdentifier(identifier, tenantId())
                .orElseThrow(() -> new UserNotFoundException("User not found: " + identifier));

        if (!privilegesChecker.isAuthorized(requester, target)) {
            throw new UnauthorizedAccessException("You do not have permission to " + action + " this user's documents");
        }
        return target;
    }

    /* ====================== SAVE FILES ====================== */
    public Map<String, String> saveFiles(List<FIleUploadDTO> fileDTOs, String folderName) throws IOException {

        Map<String, String> urls = new HashMap<>();

        Path userDir = fileStorageService.initDirectory(
                fileStorageService.userRoot().resolve(folderName)
        );

        for (FIleUploadDTO fileDTO : fileDTOs) {

            MultipartFile actualFile = fileDTO.getFile();
            if (actualFile.isEmpty()) continue;

            checkDiskSpace(userDir, actualFile.getSize());

            String fileName =
                    UUID.randomUUID()
                            + "_"
                            + actualFile.getOriginalFilename();

            Path saved;

            try (InputStream in = actualFile.getInputStream()) {

                saved = fileStorageService.saveFile(userDir, fileName, in);

                fileStorageService.secure(saved);
            }

        /* =========================
           TRACK FILE FOR ROLLBACK
           ========================= */

            transactionalFileManager.track(saved);

            urls.put(
                    "/api/users/images/" + folderName + "/" + fileName,
                    fileDTO.getDescription()
            );
        }

        return urls;
    }

    private void checkDiskSpace(Path path, long fileSize) {
        if (path.toFile().getFreeSpace() < fileSize) throw new StorageFullException("Not enough disk space to save file");
    }

    @Transactional
    public ResponseEntity<?> setProfileThumbnail(
            String identifier,
            String filename,
            Authentication authentication
    ) {

        User target = validateAccess(
                identifier,
                authentication,
                "set thumbnail"
        );

        UserImage selected =
                userImageRepository
                        .findByUserAndFileName(
                                target,
                                filename
                        )
                        .orElseThrow(() ->
                                new ImageNotFoundException(
                                        "Image not found: " + filename
                                )
                        );

        if (Boolean.TRUE.equals(selected.getDeleted())) {
            throw new ImageAccessDeniedException(
                    "Deleted image cannot be profile thumbnail"
            );
        }

        // clear old thumbnail
        userImageRepository.clearExistingThumbnail(target);
        selected.setProfileThumbnail(true);

        // promote selected
        selected.setProfileThumbnail(true);

        userImageRepository.save(selected);

        userImageAuditRepository.save(
                UserImageAudit.builder()
                        .userId(target.getId())
                        .username(target.getUsername())
                        .fileName(selected.getFileName())
                        .filePath(selected.getFilePath())
                        .action("SET_PROFILE_THUMBNAIL")
                        .performedById(
                                privilegesChecker
                                        .getAuthenticatedUser(authentication)
                                        .getId()
                        )
                        .performedByUsername(
                                privilegesChecker
                                        .getAuthenticatedUser(authentication)
                                        .getUsername()
                        )
                        .timestamp(LocalDateTime.now())
                        .build()
        );

        evictUsersAfterCommit();

        return ResponseEntity.ok(Map.of(
                "message", "Profile thumbnail updated"
        ));
    }

    @Transactional
    public ResponseEntity<?> updateDescription(
            String identifier,
            String filename,
            String description,
            Authentication authentication
    ) {

        User target = validateAccess(
                identifier,
                authentication,
                "update image description"
        );

        UserImage img = userImageRepository
                .findByUserAndFileName(target, filename)
                .orElseThrow(() ->
                        new ImageNotFoundException(
                                "Image not found: " + filename
                        )
                );

        if (Boolean.TRUE.equals(img.getDeleted())) {
            throw new ImageAccessDeniedException(
                    "Cannot update description of deleted image"
            );
        }

        String oldDescription = img.getFileDescription();

        img.setFileDescription(description);

        // audit
        userImageAuditRepository.save(
                UserImageAudit.builder()
                        .userId(target.getId())
                        .username(target.getUsername())
                        .fileName(img.getFileName())
                        .filePath(img.getFilePath())
                        .action("UPDATE_DESCRIPTION")
                        .reason("From: " + oldDescription + " → To: " + description)
                        .performedById(
                                privilegesChecker
                                        .getAuthenticatedUser(authentication)
                                        .getId()
                        )
                        .performedByUsername(
                                privilegesChecker
                                        .getAuthenticatedUser(authentication)
                                        .getUsername()
                        )
                        .timestamp(LocalDateTime.now())
                        .build()
        );

        // ✅ CRITICAL: invalidate cache
        evictUsersAfterCommit();

        return ResponseEntity.ok(Map.of(
                "message", "Description updated"
        ));
    }

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

    /* ====================== RETRIEVE / DOWNLOAD / DELETE ====================== */

    public List<String> getAllUsersImages(Boolean deletedImages,
                                          Boolean deletedUsers,
                                          Authentication authentication) {

        User performedBy = privilegesChecker.getAuthenticatedUser(authentication);

        // Fetch all images *once*
        Pageable pageable = PageRequest.of(0, 500);

        Page<UserImage> page;

        List<UserImage> authorizedImages = new ArrayList<>();

        do {

            page = userImageRepository.findByTenantIdWithUser(
                    tenantId(),
                    pageable
            );

            page.getContent().stream()
                    .filter(img -> {

                        boolean userDeletedMatch =
                                deletedUsers == null ||
                                        Boolean.TRUE.equals(img.getUser().getDeleted()) == deletedUsers;

                        boolean imageDeletedMatch =
                                deletedImages == null ||
                                        Boolean.TRUE.equals(img.getDeleted()) == deletedImages;

                        boolean authorized =
                                privilegesChecker.isAuthorized(performedBy, img.getUser());

                        return userDeletedMatch && imageDeletedMatch && authorized;
                    })
                    .forEach(authorizedImages::add);

            pageable = page.nextPageable();

        } while (page.hasNext());

        if (authorizedImages.isEmpty()) {
            throw new ImageNotFoundException("No accessible images found matching your filters.");
        }

        // Audit retrieval
//        userImageAuditRepository.save(UserImageAudit.builder()
//                .userId(null)
//                .username("all users")
//                .fileName("all images")
//                .filePath("all images")
//                .action("RETRIEVE")
//                .performedById(performedBy.getId())
//                .performedByUsername(performedBy.getUsername())
//                .timestamp(LocalDateTime.now())
//                .build());

        return authorizedImages.stream()
                .map(UserImage::getFilePath)
                .toList();
    }

    public ResponseEntity<Resource> downloadAllUsersImages(
            Boolean deletedUsers,
            Boolean deletedImages,
            Authentication authentication
    ) throws IOException {

        User performedBy = privilegesChecker.getAuthenticatedUser(authentication);

        Pageable pageable = PageRequest.of(0, 500);
        Page<UserImage> page;

        List<UserImage> authorizedImages = new ArrayList<>();

        do {

            page = userImageRepository.findByTenantIdWithUser(
                    tenantId(),
                    pageable
            );

            page.getContent().stream()
                    .filter(img -> {

                        boolean userDeletedMatch =
                                deletedUsers == null ||
                                        Boolean.TRUE.equals(img.getUser().getDeleted()) == deletedUsers;

                        boolean imageDeletedMatch =
                                deletedImages == null ||
                                        Boolean.TRUE.equals(img.getDeleted()) == deletedImages;

                        boolean authorized =
                                privilegesChecker.isAuthorized(performedBy, img.getUser());

                        return userDeletedMatch && imageDeletedMatch && authorized;
                    })
                    .forEach(authorizedImages::add);

            pageable = page.nextPageable();

        } while (page.hasNext());

        if (authorizedImages.isEmpty()) {
            throw new ImageNotFoundException("No accessible images found matching your filters.");
        }

        File tempZip = File.createTempFile("all-images-", ".zip");

        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(tempZip))) {

            for (UserImage image : authorizedImages) {

                Path userDir = fileStorageService.userRoot()
                        .resolve(image.getUser().getUploadFolder())
                        .normalize();

                Path filePath = userDir.resolve(image.getFileName()).normalize();

                if (!filePath.startsWith(userDir)) {
                    throw new SecurityException("Invalid file path");
                }

                if (!Files.exists(filePath) || !Files.isRegularFile(filePath)) {
                    log.warn("Missing image file: {}", image.getFileName());
                    continue;
                }

                zos.putNextEntry(new ZipEntry(image.getFileName()));

                try (InputStream is = Files.newInputStream(filePath)) {
                    is.transferTo(zos);
                }

                zos.closeEntry();

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

        ResponseEntity<Resource> response =
                ResponseEntity.ok()
                        .contentType(MediaType.APPLICATION_OCTET_STREAM)
                        .header(HttpHeaders.CONTENT_DISPOSITION,
                                "attachment; filename=\"all-images.zip\"")
                        .body(resource);

        tempZip.deleteOnExit();

        return response;
    }

    @Transactional(readOnly = true)
    public List<UserImageDTO> getAllUserImagesForAUser(
            String identifier,
            Authentication authentication,
            Boolean deleted
    ) {

        User target = validateAccess(
                identifier,
                authentication,
                "view"
        );

        List<UserImage> images =
                userImageRepository.findImagesForUser(
                        tenantId(),
                        target.getId(),
                        deleted
                );

        if(images.isEmpty()){
            return List.of();
        }

        User actor =
                privilegesChecker.getAuthenticatedUser(
                        authentication
                );

//        userImageAuditRepository.save(
//                UserImageAudit.builder()
//                        .userId(target.getId())
//                        .username(target.getUsername())
//                        .fileName("ALL")
//                        .filePath("ALL")
//                        .action("RETRIEVE")
//                        .performedById(actor.getId())
//                        .performedByUsername(actor.getUsername())
//                        .timestamp(LocalDateTime.now())
//                        .build()
//        );

        return images.stream()
                .map(UserImageMapper::toDto)
                .toList();
    }

    public ResponseEntity<Resource> downloadAllUserImages(
            String identifier,
            Authentication authentication,
            Boolean deleted
    ) throws IOException {

        User target = validateAccess(identifier, authentication, "download");

        List<UserImage> images = userImageRepository.findByUser(target);

        if (images.isEmpty()) {
            throw new ImageNotFoundException("User has no images: " + target.getUsername());
        }

        List<UserImage> filteredImages = images.stream()
                .filter(image -> {
                    if (deleted == null) return true;
                    if (deleted) return Boolean.TRUE.equals(image.getDeleted());
                    return !Boolean.TRUE.equals(image.getDeleted());
                })
                .toList();

        if (filteredImages.isEmpty()) {
            throw new ImageNotFoundException(
                    "No images match deleted=" + deleted + " for user: " + target.getUsername()
            );
        }

        Path userDir = fileStorageService.userRoot()
                .resolve(target.getUploadFolder())
                .normalize();

        File tempZip = File.createTempFile("user-images-", ".zip");

        try {

            try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(tempZip))) {

                for (UserImage image : filteredImages) {

                    Path filePath = userDir.resolve(image.getFileName()).normalize();

                    if (!filePath.startsWith(userDir)) {
                        throw new SecurityException("Invalid file path");
                    }

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
        Path userDir = fileStorageService.userRoot()
                .resolve(target.getUploadFolder())
                .normalize();
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

    @Transactional
    public ResponseEntity<?> softdeleteUserImage(String identifier, String filename, Authentication authentication, String reason) throws IOException {
        User target = validateAccess(identifier, authentication, "delete");

        userImageRepository.findByUserAndFileName(target, filename).ifPresent(image -> {

            boolean wasThumbnail = Boolean.TRUE.equals(image.getProfileThumbnail());

            image.setDeleted(true);
            image.setDeletedAt(LocalDateTime.now());
            image.setProfileThumbnail(false);

            userImageRepository.save(image);

            if (wasThumbnail) {
                maintainThumbnailInvariant(target);
            }

            userImageAuditRepository.save(UserImageAudit.builder()
                    .userId(target.getId())
                    .username(target.getUsername())
                    .fileName(image.getFileName())
                    .filePath(image.getFilePath())
                    .action("SOFT_DELETE")
                    .performedById(privilegesChecker.getAuthenticatedUser(authentication).getId())
                    .reason(reason)
                    .performedByUsername(privilegesChecker.getAuthenticatedUser(authentication).getUsername())
                    .timestamp(LocalDateTime.now())
                    .build());
        });

        evictUsersAfterCommit();

        return ResponseEntity.ok("Image deleted successfully: " + filename);
    }

    @Transactional
    public ResponseEntity<?> softdeleteAllUserImages(String identifier, Authentication authentication) throws IOException {
        User target = validateAccess(identifier, authentication, "delete");

        List<UserImage> images = userImageRepository.findByUser(target);
        LocalDateTime now = LocalDateTime.now();
        UUID adminId = privilegesChecker.getAuthenticatedUser(authentication).getId();
        String adminUsername = privilegesChecker.getAuthenticatedUser(authentication).getUsername();

        images.forEach(image -> {
            image.setDeleted(true);
            image.setDeletedAt(now);
            image.setProfileThumbnail(false);
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

        evictUsersAfterCommit();

        return ResponseEntity.ok("All user images deleted for user: " + target.getUploadFolder());
    }

    @Transactional
    public ResponseEntity<?> restoreUserImage(String identifier, String filename, Authentication authentication, String reason) throws IOException {
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
                .reason(reason)
                .performedById(privilegesChecker.getAuthenticatedUser(authentication).getId())
                .performedByUsername(privilegesChecker.getAuthenticatedUser(authentication).getUsername())
                .timestamp(LocalDateTime.now())
                .build());

        maintainThumbnailInvariant(target);
        evictUsersAfterCommit();

        return ResponseEntity.ok("Image restored successfully: " + filename);
    }

    @Transactional
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

        maintainThumbnailInvariant(target);
        evictUsersAfterCommit();

        return ResponseEntity.ok("All soft-deleted images restored for user: " + target.getUploadFolder());
    }

    @Transactional
    public ResponseEntity<?> harddeleteUserImage(String identifier, String filename, Authentication authentication, String reason) throws IOException {
        User target = validateAccess(identifier, authentication, "delete");
        Path userDir = fileStorageService.userRoot()
                .resolve(target.getUploadFolder())
                .normalize();

        Path filePath = userDir.resolve(filename).normalize();

        if (!filePath.startsWith(userDir)) {
            throw new SecurityException("Invalid file path");
        }
        if (!Files.exists(filePath) || !Files.isRegularFile(filePath)) {
            throw new DirectoryNotFoundException("User", target.getUsername(), filePath.toString());
        }

        // schedule deletion AFTER COMMIT
        transactionalFileManager.runAfterCommit(() -> {
            try {
                Files.deleteIfExists(filePath);

                try (var files = Files.list(userDir)) {
                    if (!files.findAny().isPresent()) {
                        Files.deleteIfExists(userDir);
                    }
                }

                log.info("File deleted after commit: {}", filePath);

            } catch (Exception e) {
                log.error("Failed to delete file after commit: {}", filePath, e);
            }
        });


        // Delete record & audit
        userImageRepository.findByUserAndFileName(target, filename).ifPresent(image -> {
            boolean wasThumbnail = image.getProfileThumbnail();
            userImageRepository.delete(image);
            userImageAuditRepository.save(UserImageAudit.builder()
                    .userId(target.getId())
                    .username(target.getUsername())
                    .fileName(image.getFileName())
                    .filePath(image.getFilePath())
                    .action("DELETE")
                    .performedById(privilegesChecker.getAuthenticatedUser(authentication).getId())
                    .reason(reason)
                    .performedByUsername(privilegesChecker.getAuthenticatedUser(authentication).getUsername())
                    .timestamp(LocalDateTime.now())
                    .build());
            if (wasThumbnail) {
                maintainThumbnailInvariant(target);
            }
        });

        evictUsersAfterCommit();

        return ResponseEntity.ok("Image deleted successfully: " + filename);
    }

    @Transactional
    public ResponseEntity<?> harddeleteAllUserImages(String identifier, Authentication authentication) throws IOException {
        User target = validateAccess(identifier, authentication, "delete");
        Path userDir = fileStorageService.userRoot()
                .resolve(target.getUploadFolder())
                .normalize();

        if (!Files.exists(userDir) || !Files.isDirectory(userDir)) {
            throw new DirectoryNotFoundException("User", target.getUsername(), userDir.toString());
        }

        transactionalFileManager.runAfterCommit(() -> {
            try {
                deleteUserUploadDirectory(userDir);
                log.info("User directory deleted after commit: {}", userDir);
            } catch (Exception e) {
                log.error("Failed deleting user directory after commit: {}", userDir, e);
            }
        });

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

        evictUsersAfterCommit();

        return ResponseEntity.ok("All images and upload directory deleted successfully for user: " + target.getUploadFolder());
    }

    public void deleteUserUploadDirectory(Path userDir) throws IOException {
        fileStorageService.deleteDirectory(userDir);
    }

    private void maintainThumbnailInvariant(User user) {

        List<UserImage> activeImages =
                userImageRepository.findByUser(user)
                        .stream()
                        .filter(i -> !Boolean.TRUE.equals(i.getDeleted()))
                        .toList();

        if (activeImages.isEmpty()) {
            return; // nothing to assign
        }

        List<UserImage> thumbnails =
                activeImages.stream()
                        .filter(i -> Boolean.TRUE.equals(i.getProfileThumbnail()))
                        .toList();

        // ❗ Case 1: multiple thumbnails → fix
        if (thumbnails.size() > 1) {
            UserImage keep = thumbnails.get(0);

            thumbnails.forEach(img -> img.setProfileThumbnail(false));
            keep.setProfileThumbnail(true);

            userImageRepository.saveAll(thumbnails);
            return;
        }

        // ❗ Case 2: no thumbnail → assign one
        if (thumbnails.isEmpty()) {

            UserImage fallback =
                    activeImages.stream()
                            .filter(i -> "passport".equalsIgnoreCase(i.getFileDescription()))
                            .findFirst()
                            .orElse(activeImages.get(0));

            fallback.setProfileThumbnail(true);
            userImageRepository.save(fallback);
        }
    }
}