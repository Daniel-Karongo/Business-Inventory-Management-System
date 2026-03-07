package com.IntegrityTechnologies.business_manager.modules.platform.tenant.model;

import jakarta.persistence.Column;
import jakarta.persistence.MappedSuperclass;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.UUID;

@MappedSuperclass
@Getter
@Setter
@SuperBuilder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
public abstract class AuditableTenantEntity extends TenantAwareEntity {

    @Column(name = "created_at")
    protected LocalDateTime createdAt;

    @Column(name = "created_by")
    protected UUID createdBy;

    @Column(name = "updated_at")
    protected LocalDateTime updatedAt;

    @Column(name = "updated_by")
    protected UUID updatedBy;

}