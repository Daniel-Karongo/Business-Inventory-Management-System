package com.IntegrityTechnologies.business_manager.modules.platform.tenant.service;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.Tenant;

import java.util.List;
import java.util.UUID;

public interface TenantService {

    Tenant createTenant(String name, String code);

    Tenant getTenant(UUID id);

    List<Tenant> getAllTenants();

    void deactivateTenant(UUID id);

}