package com.IntegrityTechnologies.business_manager.modules.user.controller;

import com.IntegrityTechnologies.business_manager.exception.UserNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.user.dto.UserDTO;
import com.IntegrityTechnologies.business_manager.modules.user.model.Role;
import com.IntegrityTechnologies.business_manager.modules.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.user.service.UserService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.*;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

@Tag(name = "Users", description = "Operations related to user accounts and roles")
@RestController
@RequestMapping("/api/users")
@RequiredArgsConstructor
public class UserController {

    private final UserService userService;

    /* ====================== REGISTER USER ====================== */
    @Operation(summary = "Register a new user with optional ID image uploads")
    @PostMapping(value = "/register", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
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

    @Operation(summary = "Update an existing user (excluding ID)")
    @PutMapping("/{identifier}")
    public ResponseEntity<UserDTO> updateUser(
            @PathVariable String identifier,
            @RequestBody UserDTO updatedUserData,
            Authentication authentication
    ) throws IOException {
        if (authentication == null || !(authentication.getPrincipal() instanceof UserDetails userDetails)) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }

        String updaterUsername = userDetails.getUsername();
        UserDTO updatedUser = userService.updateUser(identifier, updatedUserData, updaterUsername);
        return ResponseEntity.ok(updatedUser);
    }

    /* ====================== GET USERS ====================== */
    @PreAuthorize("hasAnyRole('ADMIN', 'SUPERUSER')")
    @GetMapping("/active")
    public ResponseEntity<List<User>> getAllUsers() {
        return ResponseEntity.ok(userService.getAllUsers());
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @GetMapping("/deleted")
    public ResponseEntity<List<User>> getDeletedUsers() {
        return ResponseEntity.ok(userService.getDeletedUsers());
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @GetMapping("/all-including-deleted")
    public ResponseEntity<List<User>> getAllIncludingDeleted() {
        return ResponseEntity.ok(userService.getAllUsersIncludingDeleted());
    }

    @PreAuthorize("hasAnyRole('ADMIN', 'SUPERUSER')")
    @GetMapping("/role/{role}")
    public ResponseEntity<?> getUsersByRole(@PathVariable String role) {
        try {
            Role parsedRole = Role.valueOf(role.toUpperCase());
            return ResponseEntity.ok(userService.getUsersByRole(parsedRole));
        } catch (IllegalArgumentException e) {
            return ResponseEntity.badRequest().body("Invalid role: " + role);
        }
    }

    /* ====================== USER IMAGES ====================== */
    @PreAuthorize("hasAnyRole('ADMIN', 'SUPERUSER')")
    @GetMapping("/images/{identifier}")
    public ResponseEntity<List<String>> getUserImages(@PathVariable String identifier) {
        try {
            List<String> urls = userService.getUserImages(identifier);
            return ResponseEntity.ok(urls);
        } catch (UserNotFoundException e) {
            return ResponseEntity.status(404).body(Collections.emptyList());
        }
    }

    /* ====================== SOFT DELETE / RESTORE / HARD DELETE ====================== */
    @PreAuthorize("hasAnyRole('ADMIN', 'SUPERUSER')")
    @DeleteMapping("/soft/{id}")
    public ResponseEntity<Void> softDeleteUser(@PathVariable UUID id) {
        userService.softDeleteUser(id);
        return ResponseEntity.noContent().build();
    }

    @PreAuthorize("hasAnyRole('ADMIN', 'SUPERUSER')")
    @PutMapping("/restore/{id}")
    public ResponseEntity<Void> restoreUser(@PathVariable UUID id) throws IOException {
        userService.restoreUser(id);
        return ResponseEntity.noContent().build();
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @DeleteMapping("/permanent/{id}")
    public ResponseEntity<Void> hardDeleteUser(@PathVariable UUID id) throws IOException {
        userService.hardDeleteUser(id);
        return ResponseEntity.noContent().build();
    }
}