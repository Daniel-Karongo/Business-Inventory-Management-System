package com.IntegrityTechnologies.business_manager.modules.person.entity.user.controller;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import com.IntegrityTechnologies.business_manager.common.ImagesUploadForm;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.dto.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.service.UserImageService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.service.UserService;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

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

    @PatchMapping(
            value = "/{identifier}/images",
            consumes = MediaType.MULTIPART_FORM_DATA_VALUE
    )
    public ResponseEntity<?> updateUserImages(
            @PathVariable String identifier,

            @ModelAttribute ImagesUploadForm form,

            @RequestParam(value = "deleteOldImages", defaultValue = "false")
            boolean deleteOldImages,

            Authentication authentication
    ) throws IOException {

        return userService.updateUserImages(
                identifier,
                form.getUserImagesFiles(),
                authentication,
                deleteOldImages
        );
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
    (#deleted == false || #deleted == null) and hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER', 'SUPERVISOR')
    or 
    ( #deleted == true and hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER') )""")
    @GetMapping("/all")
    public ResponseEntity<List<UserDTO>> getAllUsers(
            @RequestParam(required = false) Boolean deleted
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
    public ResponseEntity<ApiResponse> softDeleteUser(@PathVariable UUID id, @RequestBody String reason, Authentication authentication) {
        return userService.softDeleteUser(id, authentication, reason);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @DeleteMapping("/soft/bulk")
    public ResponseEntity<ApiResponse> softDeleteUsersInBulk(@RequestBody UserBulkRestoreOrDeleteDTO dto, Authentication authentication) {
        return userService.softDeleteUsersInBulk(dto.getIds(), dto.getReason(), authentication);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @PatchMapping("/restore/{id}")
    public ResponseEntity<ApiResponse> restoreUser(@PathVariable UUID id, @RequestBody(required = false) String reason, Authentication authentication) throws IOException {
        return userService.restoreUser(id, authentication, reason);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @PatchMapping("/restore/bulk")
    public ResponseEntity<ApiResponse> restoreUsersInBulk(
            @RequestBody UserBulkRestoreOrDeleteDTO dto,
            Authentication authentication) throws IOException {

        return userService.restoreUsersInBulk(dto.getIds(), dto.getReason(), authentication);
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
            "( (#deletedUsers == true || #deletedImages == true  || #deletedUsers == null || #deletedImages == null) && hasRole('SUPERUSER') )"
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
    public ResponseEntity<List<UserImageDTO>> getAllUserImages(
            @PathVariable String identifier,
            @RequestParam(required = false) Boolean deleted,
            Authentication authentication
    ) {
        return ResponseEntity.ok(
                userImageService.getAllUserImagesForAUser(identifier, authentication, deleted)
        );
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
            @RequestParam(required=false) Boolean deleted,
            Authentication authentication
    ) throws IOException {
        return userImageService.getUserImage(identifier, filename, authentication, deleted);
    }










    @DeleteMapping("/images/all/{identifier}/soft")
    public ResponseEntity<?> softdeleteAllUserImages(@PathVariable String identifier, Authentication authentication) throws IOException {
        return userImageService.softdeleteAllUserImages(identifier, authentication);
    }

    @DeleteMapping("/images/{identifier}/{filename}/soft")
    public ResponseEntity<?> softdeleteUserImage(
            @PathVariable String identifier,
            @PathVariable String filename,
            Authentication authentication
    ) throws IOException {
        return userImageService.softdeleteUserImage(identifier, filename, authentication);
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
    public ResponseEntity<List<UserImageAuditDTO>> getUserImageAuditsTarget(
            @PathVariable String identifier
    ) {
        User user = userService.getUserByIdentifierForAudits(identifier);

        return ResponseEntity.ok(
                userService.getUserImageAuditsTarget(user.getId())
                        .stream()
                        .map(UserImageAuditDTO::from)
                        .toList()
        );
    }

    @GetMapping("/images/audits/{identifier}/doer")
    @PreAuthorize("hasRole('SUPERUSER')")
    public ResponseEntity<List<UserImageAudit>> getUserImageAuditsPerpetrated(@PathVariable String identifier) {
        User user = userService.getUserByIdentifierForAudits(identifier);
        return ResponseEntity.ok(userService.getUserImageAuditsPerpetrated(user.getId()));
    }

}