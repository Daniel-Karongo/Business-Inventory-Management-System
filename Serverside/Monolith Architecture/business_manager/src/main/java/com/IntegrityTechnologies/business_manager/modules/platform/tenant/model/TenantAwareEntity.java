package com.IntegrityTechnologies.business_manager.modules.platform.tenant.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.listener.TenantEntityListener;
import jakarta.persistence.Column;
import jakarta.persistence.EntityListeners;
import jakarta.persistence.MappedSuperclass;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.annotations.Filter;
import org.hibernate.annotations.FilterDef;
import org.hibernate.annotations.ParamDef;

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
public abstract class TenantAwareEntity {

    @Column(name = "tenant_id", nullable = false)
    private UUID tenantId;

}