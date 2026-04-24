package com.IntegrityTechnologies.business_manager.modules.platform.tenant.mapper;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.dto.TenantResponse;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.Tenant;

public class TenantMapper {

    public static TenantResponse toDto(
            Tenant tenant,
            String planCode,
            String subscriptionStatus
    ) {

        return TenantResponse.builder()
                .id(tenant.getId())
                .name(tenant.getName())
                .code(tenant.getCode())
                .status(tenant.getStatus())
                .platformTenant(tenant.isPlatformTenant())
                .planCode(planCode)
                .subscriptionStatus(subscriptionStatus)
                .createdAt(tenant.getCreatedAt())
                .build();
    }

}