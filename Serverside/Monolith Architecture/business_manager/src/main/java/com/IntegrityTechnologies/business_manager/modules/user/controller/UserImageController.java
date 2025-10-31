package com.IntegrityTechnologies.business_manager.modules.user.controller;

import com.IntegrityTechnologies.business_manager.modules.user.model.Role;
import com.IntegrityTechnologies.business_manager.modules.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.config.FileStorageProperties;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.InputStreamResource;
import org.springframework.core.io.Resource;
import org.springframework.http.*;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.*;

import java.io.*;
import java.nio.file.*;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

@RestController
@RequestMapping("/api/users/images")
@RequiredArgsConstructor
public class UserImageController {

    private final UserRepository userRepository;
    private final FileStorageProperties fileStorageProperties;

    /** Fetch a specific user image */
    @GetMapping("/{identifier}/{filename:.+}")
    public ResponseEntity<Resource> getUserImage(
            @PathVariable String identifier,
            @PathVariable String filename,
            Authentication authentication
    ) throws IOException {
        // Ensure authentication
        if (authentication == null || !(authentication.getPrincipal() instanceof UserDetails userDetails)) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }

        // Identify requester (the logged-in user)
        User requester = userRepository.findByUsername(userDetails.getUsername()).orElse(null);
        if (requester == null) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }

        // Find the target user by any identifier (UUID, username, email, or ID number)
        User target = userRepository.findByIdentifier(identifier).orElse(null);
        if (target == null) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).build();
        }

        // Authorization check
        boolean authorized = requester.getRole() == Role.SUPERUSER ||
                requester.getRole() == Role.ADMIN ||
                requester.getUsername().equalsIgnoreCase(target.getUsername());

        if (!authorized) {
            return ResponseEntity.status(HttpStatus.FORBIDDEN).build();
        }

        // Construct the file path
        Path baseDir = Paths.get(fileStorageProperties.getUserUploadDir()).toAbsolutePath().normalize();
        Path filePath = baseDir.resolve(target.getUsername()).resolve(filename).normalize();

        // Validate the file path and file existence
        if (!filePath.startsWith(baseDir)) {
            return ResponseEntity.status(HttpStatus.FORBIDDEN).build();
        }
        if (!Files.exists(filePath) || !Files.isRegularFile(filePath)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).build();
        }

        // Determine content type
        String contentType = Files.probeContentType(filePath);
        if (contentType == null) contentType = "application/octet-stream";

        // Serve file as Resource
        Resource resource = new org.springframework.core.io.FileSystemResource(filePath.toFile());

        return ResponseEntity.ok()
                .contentType(MediaType.parseMediaType(contentType))
                .header(HttpHeaders.CONTENT_DISPOSITION, "inline; filename=\"" + filePath.getFileName() + "\"")
                .body(resource);
    }


    /** Download all user images as a zip */
    @GetMapping("/all/download/{identifier}")
    public ResponseEntity<Resource> downloadAllUserImages(
            @PathVariable String identifier,
            Authentication authentication
    ) throws IOException {
        if (authentication == null || !(authentication.getPrincipal() instanceof UserDetails userDetails)) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }

        User requester = userRepository.findByUsername(userDetails.getUsername()).orElse(null);
        User target = userRepository.findByIdentifier(identifier).orElse(null);

        if (target == null) return ResponseEntity.status(HttpStatus.NOT_FOUND).build();

        boolean authorized = requester != null && (
                requester.getRole() == Role.SUPERUSER ||
                        requester.getRole() == Role.ADMIN ||
                        requester.getUsername().equalsIgnoreCase(target.getUsername())
        );

        if (!authorized) return ResponseEntity.status(HttpStatus.FORBIDDEN).build();

        Path baseDir = Paths.get(fileStorageProperties.getUserUploadDir()).toAbsolutePath().normalize();
        Path userDir = baseDir.resolve(target.getUsername()).normalize();

        if (!Files.exists(userDir) || !Files.isDirectory(userDir)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).build();
        }

        // Create temp zip file
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

    /** Fetch all image URLs (metadata only) */
    @GetMapping("/all/{identifier}")
    public ResponseEntity<List<String>> getAllUserImages(
            @PathVariable String identifier,
            Authentication authentication
    ) {
        if (authentication == null || !(authentication.getPrincipal() instanceof UserDetails userDetails)) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }

        User requester = userRepository.findByUsername(userDetails.getUsername()).orElse(null);
        User target = userRepository.findByIdentifier(identifier).orElse(null);

        if (target == null) return ResponseEntity.status(HttpStatus.NOT_FOUND).build();

        boolean authorized = requester != null && (
                requester.getRole() == Role.SUPERUSER ||
                        requester.getRole() == Role.ADMIN ||
                        requester.getUsername().equalsIgnoreCase(target.getUsername())
        );

        if (!authorized) return ResponseEntity.status(HttpStatus.FORBIDDEN).build();

        return ResponseEntity.ok(target.getIdImageUrls());
    }
}