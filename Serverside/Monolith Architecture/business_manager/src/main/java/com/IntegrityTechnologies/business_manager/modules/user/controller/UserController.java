package com.IntegrityTechnologies.business_manager.modules.user.controller;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import com.IntegrityTechnologies.business_manager.modules.user.dto.UserDTO;
import com.IntegrityTechnologies.business_manager.modules.user.model.*;
import com.IntegrityTechnologies.business_manager.modules.user.service.UserImageService;
import com.IntegrityTechnologies.business_manager.modules.user.service.UserService;
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
        UserDTO savedUser = userService.registerUser(userDTO, authentication);
        return ResponseEntity.ok(savedUser);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @PostMapping(
        value = "/register/bulk",
        consumes = MediaType.MULTIPART_FORM_DATA_VALUE,
        produces = MediaType.APPLICATION_JSON_VALUE
    )
    public ResponseEntity<List<UserDTO>> registerUsersInBulk(
            @ModelAttribute UserBulkWithFilesDTO bulkDTO,
            Authentication authentication
    ) throws IOException {
        List<UserDTO> savedUsers = new ArrayList<>();
        for (UserDTO userDto : bulkDTO.getUsers()) {
            savedUsers.add(userService.registerUser(userDto, authentication));
        }

        return ResponseEntity.status(HttpStatus.CREATED).body(savedUsers);
    }

    @PatchMapping("/{identifier}")
    public ResponseEntity<UserDTO> updateUser(
            @PathVariable String identifier,
            @RequestBody UserDTO updatedData,
            Authentication authentication
    ) throws IOException {
        UserDTO dto = userService.updateUser(identifier, updatedData, authentication);
        return ResponseEntity.ok(dto);
    }

    @PatchMapping("/{identifier}/images")
    public ResponseEntity<?> updateUserImages(
            @PathVariable String identifier,
            @RequestParam(value = "userImagesFiles", required = false) List<MultipartFile> userImagesFiles,
            @RequestParam(value = "deleteOldImages") Boolean deleteOldImages,
            Authentication authentication
    ) throws IOException {
        return userService.updateUserImages(identifier, userImagesFiles, authentication, deleteOldImages);
    }










    /* ====================== GET USERS ====================== */

    @PreAuthorize("""
    #deleted == false 
    or 
    ( #deleted == true and hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER') )""")
    @GetMapping("/user/{identifier}")
    public ResponseEntity<UserDTO> getUser(
            @PathVariable String identifier,
            @RequestParam Boolean deleted
    ) {
        return ResponseEntity.ok(userService.getUser(identifier, deleted));
    }

    @PreAuthorize("""
    #deleted == false and hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER', 'SUPERVISOR')
    or 
    ( #deleted == true and hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER') )""")
    @GetMapping("all/active")
    public ResponseEntity<List<UserDTO>> getAllUsers(
            @RequestParam Boolean deleted
    ) {
        return ResponseEntity.ok(userService.getAllUsers(deleted));
    }


    @PreAuthorize("""
    #deleted == false and hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER', 'SUPERVISOR')
    or 
    ( #deleted == true and hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER') )""")
    @GetMapping("/role/{role}/active")
    public ResponseEntity<List<UserDTO>> getUsersByRole(
            @PathVariable String role,
            @RequestParam Boolean deleted
    ) {
        Role parsedRole = Role.valueOf(role.toUpperCase());
        return ResponseEntity.ok(userService.getUsersByRole(parsedRole, deleted));
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

    @PreAuthorize(
            "( (#deletedUsers == false && #deletedImages == false) && hasAnyRole('SUPERUSER','ADMIN','MANAGER') )"
                    + " or " +
            "( (#deletedUsers == true || #deletedImages == true) && hasRole('SUPERUSER') )"
    )
    @GetMapping("/images/all")
    public ResponseEntity<List<String>> getAllUsersImages(
            @RequestParam(required = false) Boolean deletedImages,
            @RequestParam(required = false) Boolean deletedUsers,
            Authentication authentication
    ) {
        return ResponseEntity.ok(
                userImageService.getAllUsersImages(deletedImages, deletedUsers, authentication)
        );
    }


    @PreAuthorize(
            "( (#deletedUsers == false && #deletedImages == false) && hasAnyRole('SUPERUSER','ADMIN','MANAGER') )"
                    + " or " +
            "( (#deletedUsers == true || #deletedImages == true) && hasRole('SUPERUSER') )"
    )
    @GetMapping("/images/all/download")
    public ResponseEntity<Resource> downloadAllUsersImages(
            @RequestParam(required = false) Boolean deletedImages,
            @RequestParam(required = false) Boolean deletedUsers,
            Authentication authentication
    ) throws IOException {
        return userImageService.downloadAllUsersImages(deletedUsers, deletedImages, authentication);
    }





    @GetMapping("/images/all/{identifier}")
    public ResponseEntity<List<String>> getAllUserImages(
            @PathVariable String identifier,
            @RequestParam(required = false) Boolean deleted,
            Authentication authentication
    ) {
        return ResponseEntity.ok(userImageService.getAllUserImagesForAUser(identifier, authentication, deleted));
    }

    @GetMapping("/images/all/download/{identifier}")
    public ResponseEntity<Resource> downloadAllUserImages(
            @PathVariable String identifier,
            @RequestParam(required = false) Boolean deleted,
            Authentication authentication
    ) throws IOException {
        return userImageService.downloadAllUserImages(identifier, authentication, deleted);
    }




    @GetMapping("/images/{identifier}/{filename:.+}")
    public ResponseEntity<Resource> getUserImage(
            @PathVariable String identifier,
            @PathVariable String filename,
            @RequestParam Boolean deleted,
            Authentication authentication
    ) throws IOException {
        return userImageService.getUserImage(identifier, filename, authentication, deleted);
    }










    @DeleteMapping("/images/all/{identifier}/soft")
    public ResponseEntity<?> softdeleteAllUserImages(@PathVariable String identifier, Authentication authentication) throws IOException {
        return userImageService.softdeleteAllUserImages(identifier, authentication);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @PatchMapping("/images/{identifier}/{filename:.+}/restore")
    public ResponseEntity<?> restoreUserImage(
            @PathVariable String identifier,
            @PathVariable String filename,
            Authentication authentication) throws IOException {
        return userImageService.restoreUserImage(identifier, filename, authentication);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @PatchMapping("/images/all/{identifier}/restore")
    public ResponseEntity<?> restoreAllUserImages(
            @PathVariable String identifier,
            Authentication authentication) throws IOException {
        return userImageService.restoreAllUserImages(identifier, authentication);
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @DeleteMapping("/images/{identifier}/{filename:.+}/hard")
    public ResponseEntity<?> harddeleteUserImage(@PathVariable String identifier, @PathVariable String filename, Authentication authentication) throws IOException {
        return userImageService.harddeleteUserImage(identifier, filename, authentication);
    }

    @PreAuthorize("hasRole('SUPERUSER')")
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