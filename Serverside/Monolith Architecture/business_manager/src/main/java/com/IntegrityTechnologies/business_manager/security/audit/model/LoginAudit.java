package com.IntegrityTechnologies.business_manager.security.audit.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.TenantAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "login_audit")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
public class LoginAudit extends TenantAwareEntity {

    @Id
    @GeneratedValue
    private UUID id;

    private UUID userId;
    private UUID branchId;

    private String fingerprint;

    private Double latitude;
    private Double longitude;
    private Double accuracy;

    private String ip;

    private String status;
    private String reason;

    private LocalDateTime timestamp;

    @PrePersist
    void onCreate() {
        timestamp = LocalDateTime.now();
    }
}