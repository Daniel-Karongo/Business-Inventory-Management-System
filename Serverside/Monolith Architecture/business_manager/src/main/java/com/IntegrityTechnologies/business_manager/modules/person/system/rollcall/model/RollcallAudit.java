package com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "rollcall_audits")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class RollcallAudit extends BranchAwareEntity {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    private UUID rollcallId;
    private UUID userId;
    private UUID departmentId;

    @Enumerated(EnumType.STRING)
    private RollcallMethod authMethod;

    private String action;
    private String reason;

    private LocalDateTime timestamp;

    private String performedBy;

    @PrePersist
    void onCreate() {
        if (timestamp == null) {
            timestamp = LocalDateTime.now();
        }
    }
}