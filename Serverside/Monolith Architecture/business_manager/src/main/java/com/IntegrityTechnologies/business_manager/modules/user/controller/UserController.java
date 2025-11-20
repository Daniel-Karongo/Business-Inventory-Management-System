package com.IntegrityTechnologies.business_manager.modules.user.controller;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import com.IntegrityTechnologies.business_manager.exception.UserNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.category.dto.CategoryDTO;
import com.IntegrityTechnologies.business_manager.modules.user.dto.UserDTO;
import com.IntegrityTechnologies.business_manager.modules.user.model.*;
import com.IntegrityTechnologies.business_manager.modules.user.service.UserImageService;
import com.IntegrityTechnologies.business_manager.modules.user.service.UserService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.ArrayList;
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

    @PostMapping(
        value = "/register/bulk",
        consumes = MediaType.MULTIPART_FORM_DATA_VALUE,
        produces = MediaType.APPLICATION_JSON_VALUE
    )
    public ResponseEntity<List<UserDTO>> registerUsersInBulk(
            @ModelAttribute UserBulkWithFilesDTO bulkDTO,
            Authentication authentication
    ) throws IOException {

        String creatorUsername = (authentication != null && authentication.getPrincipal() instanceof UserDetails ud)
                ? ud.getUsername()
                : null;

        List<UserDTO> savedUsers = new ArrayList<>();
        for (UserDTO userDto : bulkDTO.getUsers()) {
            savedUsers.add(userService.registerUser(userDto, creatorUsername));
        }

        return ResponseEntity.status(HttpStatus.CREATED).body(savedUsers);
    }

    @PatchMapping("/{identifier}")
    public ResponseEntity<UserDTO> updateUser(
            @PathVariable String identifier,
            @RequestBody UserDTO updatedData,
            Authentication authentication
    ) throws IOException {
        UserDTO dto = userService.updateUser(identifier, updatedData, authentication.getName());
        return ResponseEntity.ok(dto);
    }

    @PatchMapping("/{identifier}/images")
    public ResponseEntity<?> updateUserImages(
            @PathVariable String identifier,
            @RequestParam(value = "userImagesFiles", required = false) List<MultipartFile> userImagesFiles,
            @RequestParam(value = "deleteOldImages", required = true) Boolean deleteOldImages,
            Authentication authentication
    ) throws IOException {
        return userService.updateUserImages(identifier, userImagesFiles, authentication, deleteOldImages);
    }










    /* ====================== GET USERS ====================== */

    @GetMapping("/user/{identifier}/active")
    public ResponseEntity<UserDTO> getActiveUser(@PathVariable String identifier) {
        return ResponseEntity.ok(userService.getActiveUser(identifier));
    }

    @GetMapping("/user/{identifier}/any")
    @PreAuthorize("hasRole('SUPERUSER')")
    public ResponseEntity<UserDTO> getActiveOrDeletedUser(@PathVariable String identifier) {
        return ResponseEntity.ok(userService.getActiveOrDeletedUser(identifier));
    }

    @PreAuthorize("hasAnyRole('ADMIN', 'SUPERUSER')")
    @GetMapping("all/active")
    public ResponseEntity<List<UserDTO>> getAllActiveUsers() {
        return ResponseEntity.ok(userService.getAllActiveUsers());
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @GetMapping("all/deleted")
    public ResponseEntity<List<UserDTO>> getAllDeletedUsers() {
        return ResponseEntity.ok(userService.getAllDeletedUsers());
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @GetMapping("/all")
    public ResponseEntity<List<UserDTO>> getAllIncludingDeleted() {
        return ResponseEntity.ok(userService.getAllUsersIncludingDeleted());
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER', 'SUPERVISOR')")
    @GetMapping("/role/{role}/active")
    public ResponseEntity<List<UserDTO>> getActiveUsersByRole(@PathVariable String role) {
        Role parsedRole = Role.valueOf(role.toUpperCase());
        return ResponseEntity.ok(userService.getUsersByRole(parsedRole, false));
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER', 'SUPERVISOR')")
    @GetMapping("/role/{role}/deleted")
    public ResponseEntity<List<UserDTO>> getDeletedUsersByRole(@PathVariable String role) {
        Role parsedRole = Role.valueOf(role.toUpperCase());
        return ResponseEntity.ok(userService.getUsersByRole(parsedRole, true));
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER', 'SUPERVISOR')")
    @GetMapping("/role/{role}/all")
    public ResponseEntity<List<UserDTO>> getUsersByRole(@PathVariable String role) {
        Role parsedRole = Role.valueOf(role.toUpperCase());
        return ResponseEntity.ok(userService.getUsersByRole(parsedRole, null));
    }








    /* ====================== SOFT DELETE / RESTORE / HARD DELETE ====================== */
    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @DeleteMapping("/soft/{id}")
    public ResponseEntity<ApiResponse> softDeleteUser(@PathVariable UUID id, Authentication authentication) {
        return userService.softDeleteUser(id, authentication);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @DeleteMapping("/soft/bulk")
    public ResponseEntity<ApiResponse> softDeleteUsersInBulk(@RequestBody List<UUID> userIds, Authentication authentication) {
        return userService.softDeleteUsersInBulk(userIds, authentication);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @PatchMapping("/restore/{id}")
    public ResponseEntity<ApiResponse> restoreUser(@PathVariable UUID id, Authentication authentication) throws IOException {
        return userService.restoreUser(id, authentication);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @PatchMapping("/restore/bulk")
    public ResponseEntity<ApiResponse> restoreUsersInBulk(@RequestBody List<UUID> userIds, Authentication authentication) throws IOException {
        return userService.restoreUsersInBulk(userIds, authentication);
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @DeleteMapping("/hard/{id}")
    public ResponseEntity<ApiResponse> hardDeleteUser(@PathVariable UUID id, Authentication authentication) throws IOException {
        return userService.hardDeleteUser(id, authentication);
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @DeleteMapping("/hard/bulk")
    public ResponseEntity<ApiResponse> hardDeleteUsersInBulk(@RequestBody List<UUID> userIds, Authentication authentication) throws IOException {
        return userService.hardDeleteUsersInBulk(userIds, authentication);
    }








    /* ====================== USER IMAGES ====================== */

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @GetMapping("/images/all/active")
    public ResponseEntity<List<String>> getAllActiveImages(Authentication authentication) {
        return ResponseEntity.ok(userImageService.getAllUsersImages(false, authentication));
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @GetMapping("/images/all/soft-deleted")
    public ResponseEntity<List<String>> getAllSoftDeletedImages(Authentication authentication) {
        return ResponseEntity.ok(userImageService.getAllUsersImages(true, authentication));
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @GetMapping("/images/all/any")
    public ResponseEntity<List<String>> getAllImages(Authentication authentication) {
        return ResponseEntity.ok(userImageService.getAllUsersImages(null, authentication));
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @GetMapping("/images/all/download/active")
    public ResponseEntity<Resource> downloadAllActiveImages(Authentication authentication) throws IOException {
        return userImageService.downloadAllUsersImages(false, authentication);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @GetMapping("/images/all/download/soft-deleted")
    public ResponseEntity<Resource> downloadAllSoftDeletedImages(Authentication authentication) throws IOException {
        return userImageService.downloadAllUsersImages(true, authentication);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @GetMapping("/images/all/download/all")
    public ResponseEntity<Resource> downloadAllImages(Authentication authentication) throws IOException {
        return userImageService.downloadAllUsersImages(null, authentication);
    }





    @GetMapping("/images/all/{identifier}/active")
    public ResponseEntity<List<String>> getAllActiveUserImages(@PathVariable String identifier, Authentication authentication) {
        return ResponseEntity.ok(userImageService.getAllUserImagesForAUser(identifier, authentication, false));
    }

    @GetMapping("/images/all/{identifier}/soft-deleted")
    public ResponseEntity<List<String>> getAllSoftDeletedUserImages(@PathVariable String identifier, Authentication authentication) {
        return ResponseEntity.ok(userImageService.getAllUserImagesForAUser(identifier, authentication, true));
    }

    @GetMapping("/images/all/{identifier}/all")
    public ResponseEntity<List<String>> getAllUserImages(@PathVariable String identifier, Authentication authentication) {
        return ResponseEntity.ok(userImageService.getAllUserImagesForAUser(identifier, authentication, null));
    }

    @GetMapping("/images/all/download/{identifier}/active")
    public ResponseEntity<Resource> downloadAllActiveUserImages(@PathVariable String identifier, Authentication authentication) throws IOException {
        return userImageService.downloadAllUserImages(identifier, authentication, false);
    }

    @GetMapping("/images/all/download/{identifier}/soft-deleted")
    public ResponseEntity<Resource> downloadAllSoftDeletedUserImages(@PathVariable String identifier, Authentication authentication) throws IOException {
        return userImageService.downloadAllUserImages(identifier, authentication, true);
    }

    @GetMapping("/images/all/download/{identifier}/all")
    public ResponseEntity<Resource> downloadAllUserImages(@PathVariable String identifier, Authentication authentication) throws IOException {
        return userImageService.downloadAllUserImages(identifier, authentication, null);
    }

    @GetMapping("/images/{identifier}/{filename:.+}/active")
    public ResponseEntity<Resource> getUserActiveImage(@PathVariable String identifier, @PathVariable String filename, Authentication authentication) throws IOException {
        return userImageService.getUserImage(identifier, filename, authentication, false);
    }

    @GetMapping("/images/{identifier}/{filename:.+}/soft-deleted")
    public ResponseEntity<Resource> getUserSoftDeletedImage(@PathVariable String identifier, @PathVariable String filename, Authentication authentication) throws IOException {
        return userImageService.getUserImage(identifier, filename, authentication, true);
    }

    @GetMapping("/images/{identifier}/{filename:.+}/any")
    public ResponseEntity<Resource> getUserImage(@PathVariable String identifier, @PathVariable String filename, Authentication authentication) throws IOException {
        return userImageService.getUserImage(identifier, filename, authentication, null);
    }

    @DeleteMapping("/images/{identifier}/{filename:.+}/soft")
    public ResponseEntity<?> softdeleteUserImage(@PathVariable String identifier, @PathVariable String filename, Authentication authentication) throws IOException {
        return userImageService.softdeleteUserImage(identifier, filename, authentication);
    }

    @DeleteMapping("/images/all/{identifier}/soft")
    public ResponseEntity<?> softdeleteAllUserImages(@PathVariable String identifier, Authentication authentication) throws IOException {
        return userImageService.softdeleteAllUserImages(identifier, authentication);
    }

    @PatchMapping("/images/{identifier}/{filename:.+}/restore")
    public ResponseEntity<?> restoreUserImage(
            @PathVariable String identifier,
            @PathVariable String filename,
            Authentication authentication) throws IOException {
        return userImageService.restoreUserImage(identifier, filename, authentication);
    }

    @PatchMapping("/images/all/{identifier}/restore")
    public ResponseEntity<?> restoreAllUserImages(
            @PathVariable String identifier,
            Authentication authentication) throws IOException {
        return userImageService.restoreAllUserImages(identifier, authentication);
    }

    @DeleteMapping("/images/{identifier}/{filename:.+}/hard")
    public ResponseEntity<?> harddeleteUserImage(@PathVariable String identifier, @PathVariable String filename, Authentication authentication) throws IOException {
        return userImageService.harddeleteUserImage(identifier, filename, authentication);
    }

    @DeleteMapping("/images/all/{identifier}/hard")
    public ResponseEntity<?> harddeleteAllUserImages(@PathVariable String identifier, Authentication authentication) throws IOException {
        return userImageService.harddeleteAllUserImages(identifier, authentication);
    }










    /* ====================== GET AUDITS ====================== */

    @GetMapping("/audits/{identifier}/target")
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

}