package com.IntegrityTechnologies.business_manager.modules.platform.tenant.mapper;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.dto.TenantDTO;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.Tenant;

public class TenantMapper {

    public static TenantDTO toDTO(Tenant tenant) {

        return TenantDTO.builder()
                .id(tenant.getId())
                .code(tenant.getCode())
                .name(tenant.getName())
                .status(tenant.getStatus().name())
                .platformTenant(tenant.isPlatformTenant())
                .createdAt(tenant.getCreatedAt())
                .build();

    }
}