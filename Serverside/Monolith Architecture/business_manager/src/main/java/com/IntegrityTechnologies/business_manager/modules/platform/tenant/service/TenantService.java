package com.IntegrityTechnologies.business_manager.modules.platform.tenant.service;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.dto.TenantCreateRequest;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.dto.TenantResponse;
import org.springframework.data.domain.Page;

import java.util.UUID;

public interface TenantService {

    TenantResponse createTenant(TenantCreateRequest request);

    TenantResponse getTenant(UUID id);

    Page<TenantResponse> getTenants(int page, int size);

    void deactivateTenant(UUID id);

}