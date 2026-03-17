package com.IntegrityTechnologies.business_manager.modules.platform.tenant.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.listener.TenantEntityListener;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.hibernate.annotations.*;

import java.time.LocalDateTime;
import java.util.UUID;

@MappedSuperclass
@EntityListeners(TenantEntityListener.class)

@FilterDef(
        name = "tenantFilter",
        parameters = @ParamDef(name = "tenantId", type = UUID.class)
)

@Filter(
        name = "tenantFilter",
        condition = "tenant_id = :tenantId"
)

@Getter
@Setter
@SuperBuilder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
public abstract class TenantAwareEntity {

    @Column(name = "tenant_id", nullable = false, updatable = false)
    private UUID tenantId;

    @CreationTimestamp
    @Column(name = "created_at", updatable = false, nullable = false)
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;
}