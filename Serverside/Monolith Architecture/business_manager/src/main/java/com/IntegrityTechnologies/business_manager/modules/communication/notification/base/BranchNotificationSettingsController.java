package com.IntegrityTechnologies.business_manager.modules.communication.notification.base;

import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.UUID;

@RestController
@RequestMapping("/api/settings/notifications")
@RequiredArgsConstructor
@TenantUserOnly
public class BranchNotificationSettingsController {

    private final BranchNotificationSettingsService service;

    @GetMapping("/branch/{branchId}")
    public ResponseEntity<BranchNotificationSettings> get(
            @PathVariable UUID branchId
    ) {

        return ResponseEntity.ok(
                service.get(branchId)
        );
    }

    @TenantManagerOnly
    @PutMapping("/branch/{branchId}")
    public ResponseEntity<BranchNotificationSettings> update(
            @PathVariable UUID branchId,
            @RequestBody BranchNotificationSettingsDTO dto
    ) {

        return ResponseEntity.ok(
                service.update(
                        branchId,
                        dto
                )
        );
    }
}