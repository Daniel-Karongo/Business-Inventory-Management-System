package com.IntegrityTechnologies.business_manager.modules.user.controller;

import com.IntegrityTechnologies.business_manager.modules.user.service.UserImageService;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.Resource;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import java.io.IOException;
import java.util.List;

@RestController
@RequestMapping("/api/users/images")
@RequiredArgsConstructor
public class UserImageController {

    private final UserImageService userImageService;

    @GetMapping("/all/{identifier}")
    public ResponseEntity<List<String>> getAllUserImages(
            @PathVariable String identifier,
            Authentication authentication
    ) {
        return ResponseEntity.ok(userImageService.getAllUserImages(identifier, authentication));
    }

    @GetMapping("/all/download/{identifier}")
    public ResponseEntity<Resource> downloadAllUserImages(
            @PathVariable String identifier,
            Authentication authentication
    ) throws IOException {
        return userImageService.downloadAllUserImages(identifier, authentication);
    }

    @GetMapping("/{identifier}/{filename:.+}")
    public ResponseEntity<Resource> getUserImage(
            @PathVariable String identifier,
            @PathVariable String filename,
            Authentication authentication
    ) throws IOException {
        return userImageService.getUserImage(identifier, filename, authentication);
    }

    @DeleteMapping("/{identifier}/{filename:.+}")
    public ResponseEntity<?> deleteUserImage(
            @PathVariable String identifier,
            @PathVariable String filename,
            Authentication authentication
    ) throws IOException {
        return userImageService.deleteUserImage(identifier, filename, authentication);
    }

    @DeleteMapping("/all/{identifier}")
    public ResponseEntity<?> deleteAllUserImages(
            @PathVariable String identifier,
            Authentication authentication
    ) throws IOException {
        return userImageService.deleteAllUserImages(identifier, authentication);
    }
}