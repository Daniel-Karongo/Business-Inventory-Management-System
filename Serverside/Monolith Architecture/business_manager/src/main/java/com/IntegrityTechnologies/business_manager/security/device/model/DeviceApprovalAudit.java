package com.IntegrityTechnologies.business_manager.security.device.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.TenantAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name="device_approval_audit")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class DeviceApprovalAudit extends TenantAwareEntity {

    @Id
    @GeneratedValue
    private UUID id;

    private UUID deviceId;

    private UUID actedByUserId;

    private String action;

    private String reason;

    private LocalDateTime actedAt;

    @PrePersist
    void init(){
        actedAt=LocalDateTime.now();
    }
}