package com.IntegrityTechnologies.business_manager.modules.person.user.controller;

import com.IntegrityTechnologies.business_manager.config.bulk.BulkRequest;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkResult;
import com.IntegrityTechnologies.business_manager.config.files.ImagesUploadForm;
import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.modules.person.user.dto.*;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.Role;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.UserAudit;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.UserImageAudit;
import com.IntegrityTechnologies.business_manager.modules.person.user.service.UserBulkService;
import com.IntegrityTechnologies.business_manager.modules.person.user.service.UserImageService;
import com.IntegrityTechnologies.business_manager.modules.person.user.service.UserService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.PlatformAdminOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantAdminOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

@Tag(name = "Users")
@RestController
@RequestMapping("/api/users")
@RequiredArgsConstructor
@TenantUserOnly
public class UserController {

    private final UserService userService;
    private final UserImageService userImageService;
    private final UserBulkService bulkService;

    /* ====================== IMPORT ====================== */

    @TenantAdminOnly
    @PostMapping("/import")
    public ResponseEntity<BulkResult<UserDTO>> importUsers(
            @RequestBody BulkRequest<UserBulkRow> request,
            Authentication authentication
    ) {
        return ResponseEntity.ok(
                bulkService.importUsers(request, authentication)
        );
    }

    /* ====================== REGISTER ====================== */

    @TenantAdminOnly
    @PostMapping(value="/register", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResponseEntity<UserDTO> register(
            @ModelAttribute UserDTO userDTO,
            Authentication authentication
    ) throws IOException {

        UserDTO savedUser = userService.registerUser(userDTO, authentication);

        return ResponseEntity.ok(savedUser);
    }

    /* ====================== UPDATE ====================== */

    @TenantManagerOnly
    @PatchMapping("/{identifier}")
    public ResponseEntity<UserDTO> updateUser(
            @PathVariable String identifier,
            @RequestBody UserDTO updatedData,
            Authentication authentication
    ) throws IOException {

        UserDTO dto =
                userService.updateUser(identifier, updatedData, authentication);

        return ResponseEntity.ok(dto);
    }

    /* ====================== USER IMAGES ====================== */

    @TenantManagerOnly
    @PatchMapping(
            value = "/{identifier}/images",
            consumes = MediaType.MULTIPART_FORM_DATA_VALUE
    )
    public ResponseEntity<?> updateUserImages(
            @PathVariable String identifier,
            @ModelAttribute ImagesUploadForm form,
            @RequestParam(defaultValue = "false") boolean deleteOldImages,
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

    @GetMapping("/user/{identifier}")
    public ResponseEntity<UserDTO> getUser(
            @PathVariable String identifier,
            @RequestParam Boolean deleted
    ) {

        return ResponseEntity.ok(
                userService.getUser(identifier, deleted)
        );
    }

    @GetMapping("/all")
    public ResponseEntity<Page<UserDTO>> getUsers(
            @RequestParam(required = false) Boolean deleted,
            @RequestParam(required = false) String role,
            @RequestParam(required = false) UUID branch,
            @RequestParam(required = false) UUID department,
            @RequestParam(required = false) String q,
            Pageable pageable
    ) {
        return ResponseEntity.ok(
                userService.getUsersFiltered(deleted, role, branch, department, q, pageable)
        );
    }

    @GetMapping("/role/{role}")
    public ResponseEntity<Page<UserDTO>> getUsersByRole(
            @PathVariable String role,
            @RequestParam(required = false) Boolean deleted,
            Pageable pageable
    ) {

        Role parsedRole = Role.valueOf(role.toUpperCase());

        return ResponseEntity.ok(
                userService.getUsersByRole(parsedRole, deleted, pageable)
        );
    }

    /* ====================== SOFT DELETE ====================== */

    @TenantManagerOnly
    @DeleteMapping("/soft/{id}")
    public ResponseEntity<ApiResponse> softDeleteUser(
            @PathVariable UUID id,
            @RequestBody String reason,
            Authentication authentication
    ) {

        return userService.softDeleteUser(id, authentication, reason);
    }

    @TenantManagerOnly
    @DeleteMapping("/soft/bulk")
    public ResponseEntity<ApiResponse> softDeleteUsersInBulk(
            @RequestBody UserBulkRestoreOrDeleteDTO dto,
            Authentication authentication
    ) {

        return userService.softDeleteUsersInBulk(
                dto.getIds(),
                dto.getReason(),
                authentication
        );
    }

    /* ====================== RESTORE ====================== */

    @TenantManagerOnly
    @PatchMapping("/restore/{id}")
    public ResponseEntity<ApiResponse> restoreUser(
            @PathVariable UUID id,
            @RequestBody(required = false) String reason,
            Authentication authentication
    ) throws IOException {

        return userService.restoreUser(id, authentication, reason);
    }

    @TenantManagerOnly
    @PatchMapping("/restore/bulk")
    public ResponseEntity<ApiResponse> restoreUsersInBulk(
            @RequestBody UserBulkRestoreOrDeleteDTO dto,
            Authentication authentication
    ) throws IOException {

        return userService.restoreUsersInBulk(
                dto.getIds(),
                dto.getReason(),
                authentication
        );
    }

    /* ====================== HARD DELETE ====================== */

    @PlatformAdminOnly
    @DeleteMapping("/hard/{id}")
    public ResponseEntity<ApiResponse> hardDeleteUser(
            @PathVariable UUID id,
            Authentication authentication
    ) throws IOException {

        return userService.hardDeleteUser(id, authentication);
    }

    @PlatformAdminOnly
    @DeleteMapping("/hard/bulk")
    public ResponseEntity<ApiResponse> hardDeleteUsersInBulk(
            @RequestBody List<UUID> userIds,
            Authentication authentication
    ) throws IOException {

        return userService.hardDeleteUsersInBulk(userIds, authentication);
    }

    /* ====================== USER IMAGES ====================== */

    @TenantManagerOnly
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

    @TenantManagerOnly
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

    @TenantManagerOnly
    @DeleteMapping("/images/all/{identifier}/soft")
    public ResponseEntity<?> softdeleteAllUserImages(
            @PathVariable String identifier,
            Authentication authentication
    ) throws IOException {
        return userImageService.softdeleteAllUserImages(identifier, authentication);
    }

    @TenantManagerOnly
    @DeleteMapping("/images/{identifier}/{filename}/soft")
    public ResponseEntity<?> softdeleteUserImage(
            @PathVariable String identifier,
            @PathVariable String filename,
            Authentication authentication
    ) throws IOException {
        return userImageService.softdeleteUserImage(identifier, filename, authentication);
    }

    @TenantManagerOnly
    @PatchMapping("/images/{identifier}/{filename:.+}/restore")
    public ResponseEntity<?> restoreUserImage(
            @PathVariable String identifier,
            @PathVariable String filename,
            Authentication authentication
    ) throws IOException {
        return userImageService.restoreUserImage(identifier, filename, authentication);
    }

    @TenantManagerOnly
    @PatchMapping("/images/all/{identifier}/restore")
    public ResponseEntity<?> restoreAllUserImages(
            @PathVariable String identifier,
            Authentication authentication
    ) throws IOException {
        return userImageService.restoreAllUserImages(identifier, authentication);
    }

    @PlatformAdminOnly
    @DeleteMapping("/images/{identifier}/{filename:.+}/hard")
    public ResponseEntity<?> harddeleteUserImage(
            @PathVariable String identifier,
            @PathVariable String filename,
            Authentication authentication
    ) throws IOException {
        return userImageService.harddeleteUserImage(identifier, filename, authentication);
    }

    @PlatformAdminOnly
    @DeleteMapping("/images/all/{identifier}/hard")
    public ResponseEntity<?> harddeleteAllUserImages(
            @PathVariable String identifier,
            Authentication authentication
    ) throws IOException {
        return userImageService.harddeleteAllUserImages(identifier, authentication);
    }

    /* ====================== AUDITS ====================== */

    @PlatformAdminOnly
    @GetMapping("/audits/{identifier}/target")
    public ResponseEntity<Page<UserAudit>> getUserAuditsTarget(
            @PathVariable String identifier,
            Pageable pageable
    ) {

        User user = userService.getUserByIdentifierForAudits(identifier);

        return ResponseEntity.ok(
                userService.getUserAuditsTarget(user.getId(), pageable)
        );
    }

    @PlatformAdminOnly
    @GetMapping("/audits/{identifier}/doer")
    public ResponseEntity<List<UserAudit>> getUserAuditsPerpetrated(
            @PathVariable String identifier
    ) {

        User user = userService.getUserByIdentifierForAudits(identifier);

        return ResponseEntity.ok(
                userService.getUserAuditsPerpetrated(user.getId())
        );

    }

    @PlatformAdminOnly
    @GetMapping("/images/audits/{identifier}/receiver")
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

    @PlatformAdminOnly
    @GetMapping("/images/audits/{identifier}/doer")
    public ResponseEntity<List<UserImageAudit>> getUserImageAuditsPerpetrated(
            @PathVariable String identifier
    ) {

        User user = userService.getUserByIdentifierForAudits(identifier);

        return ResponseEntity.ok(
                userService.getUserImageAuditsPerpetrated(user.getId())
        );

    }

}