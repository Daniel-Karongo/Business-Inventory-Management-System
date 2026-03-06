package com.IntegrityTechnologies.business_manager.modules.platform.tenant.model;

import jakarta.persistence.Column;
import jakarta.persistence.MappedSuperclass;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDateTime;
import java.util.UUID;

@MappedSuperclass
@Getter
@Setter
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