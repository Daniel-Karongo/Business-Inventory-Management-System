package com.IntegrityTechnologies.business_manager.modules.user.controller;

import com.IntegrityTechnologies.business_manager.exception.UserNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.user.dto.UserDTO;
import com.IntegrityTechnologies.business_manager.modules.user.model.Role;
import com.IntegrityTechnologies.business_manager.modules.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.user.model.UserAudit;
import com.IntegrityTechnologies.business_manager.modules.user.model.UserImageAudit;
import com.IntegrityTechnologies.business_manager.modules.user.service.UserImageService;
import com.IntegrityTechnologies.business_manager.modules.user.service.UserService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.Resource;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

@Tag(name = "Users")
@RestController
@RequestMapping("/api/users")
@RequiredArgsConstructor
public class UserController {

    private final UserService userService;
    private final UserImageService userImageService;




    /* ====================== REGISTER USER ====================== */
    @PostMapping(value="/register", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResponseEntity<UserDTO> register(
            @ModelAttribute UserDTO userDTO,
            Authentication authentication
    ) throws IOException {
        String creatorUsername = (authentication != null && authentication.getPrincipal() instanceof UserDetails userDetails)
                ? userDetails.getUsername()
                : null;

        UserDTO savedUser = userService.registerUser(userDTO, creatorUsername);
        return ResponseEntity.ok(savedUser);
    }

    @PutMapping("/{identifier}")
    public ResponseEntity<UserDTO> updateUser(
            @PathVariable String identifier,
            @RequestBody UserDTO updatedData,
            Authentication authentication
    ) throws IOException {
        UserDTO dto = userService.updateUser(identifier, updatedData, authentication.getName());
        return ResponseEntity.ok(dto);
    }

    @PutMapping("/{identifier}/images")
    public ResponseEntity<?> updateUserImages(
            @PathVariable String identifier,
            @RequestParam(value = "userImagesFiles", required = false) List<MultipartFile> userImagesFiles,
            Authentication authentication
    ) throws IOException {
        return userService.updateUserImages(identifier, userImagesFiles, authentication);
    }

    @GetMapping("/audits/{identifier}/receiver")
    @PreAuthorize("hasRole('SUPERUSER')")
    public ResponseEntity<List<UserAudit>> getUserAuditsTarget(@PathVariable String identifier) {
        User user = userService.getUserByIdentifierForAudits(identifier);
        return ResponseEntity.ok(userService.getUserAuditsTarget(user.getId()));
    }

    @GetMapping("/audits/{identifier}/doer")
    @PreAuthorize("hasRole('SUPERUSER')")
    public ResponseEntity<List<UserAudit>> getUserAuditsPerpetrated(@PathVariable String identifier) {
        User user = userService.getUserByIdentifierForAudits(identifier);
        return ResponseEntity.ok(userService.getUserAuditsPerpetrated(user.getId()));
    }

    @GetMapping("/images/audits/{identifier}/receiver")
    @PreAuthorize("hasRole('SUPERUSER')")
    public ResponseEntity<List<UserImageAudit>> getUserImageAuditsTarget(@PathVariable String identifier) {
        User user = userService.getUserByIdentifierForAudits(identifier);
        return ResponseEntity.ok(userService.getUserImageAuditsTarget(user.getId()));
    }
    @GetMapping("/images/audits/{identifier}/doer")
    @PreAuthorize("hasRole('SUPERUSER')")
    public ResponseEntity<List<UserImageAudit>> getUserImageAuditsPerpetrated(@PathVariable String identifier) {
        User user = userService.getUserByIdentifierForAudits(identifier);
        return ResponseEntity.ok(userService.getUserImageAuditsPerpetrated(user.getId()));
    }






    /* ====================== GET USERS ====================== */
    @PreAuthorize("hasAnyRole('ADMIN', 'SUPERUSER')")
    @GetMapping("/active")
    public ResponseEntity<List<UserDTO>> getAllActiveUsers() {
        return ResponseEntity.ok(userService.getAllActiveUsers());
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @GetMapping("/deleted")
    public ResponseEntity<List<UserDTO>> getDeletedUsers() {
        return ResponseEntity.ok(userService.getDeletedUsers());
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @GetMapping("/all-including-deleted")
    public ResponseEntity<List<UserDTO>> getAllIncludingDeleted() {
        return ResponseEntity.ok(userService.getAllUsersIncludingDeleted());
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER', 'SUPERVISOR')")
    @GetMapping("/role/{role}")
    public ResponseEntity<List<UserDTO>> getUsersByRole(@PathVariable String role) {
        Role parsedRole = Role.valueOf(role.toUpperCase());
        return ResponseEntity.ok(userService.getUsersByRole(parsedRole));
    }








    /* ====================== SOFT DELETE / RESTORE / HARD DELETE ====================== */
    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @DeleteMapping("/soft/{id}")
    public ResponseEntity<?> softDeleteUser(@PathVariable UUID id, Authentication authentication) {
        return userService.softDeleteUser(id, authentication);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @PutMapping("/restore/{id}")
    public ResponseEntity<?> restoreUser(@PathVariable UUID id, Authentication authentication) throws IOException {
        return userService.restoreUser(id, authentication);
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @DeleteMapping("/permanent/{id}")
    public ResponseEntity<?> hardDeleteUser(@PathVariable UUID id, Authentication authentication) throws IOException {
        return userService.hardDeleteUser(id, authentication);
    }








    /* ====================== USER IMAGES ====================== */
    @GetMapping("/images/all/{identifier}")
    public ResponseEntity<List<String>> getAllUserImages(@PathVariable String identifier, Authentication authentication) {
        return ResponseEntity.ok(userImageService.getAllUserImages(identifier, authentication));
    }

    @GetMapping("/images/all/download/{identifier}")
    public ResponseEntity<Resource> downloadAllUserImages(@PathVariable String identifier, Authentication authentication) throws IOException {
        return userImageService.downloadAllUserImages(identifier, authentication);
    }

    @GetMapping("/images/{identifier}/{filename:.+}")
    public ResponseEntity<Resource> getUserImage(@PathVariable String identifier, @PathVariable String filename, Authentication authentication) throws IOException {
        return userImageService.getUserImage(identifier, filename, authentication);
    }

    @DeleteMapping("/images/{identifier}/{filename:.+}")
    public ResponseEntity<?> deleteUserImage(@PathVariable String identifier, @PathVariable String filename, Authentication authentication) throws IOException {
        return userImageService.deleteUserImage(identifier, filename, authentication);
    }

    @DeleteMapping("/images/all/{identifier}")
    public ResponseEntity<?> deleteAllUserImages(@PathVariable String identifier, Authentication authentication) throws IOException {
        return userImageService.deleteAllUserImages(identifier, authentication);
    }
}