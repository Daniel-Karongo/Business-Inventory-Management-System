package com.IntegrityTechnologies.business_manager.modules.platform.tenant.resolver;

import jakarta.servlet.http.HttpServletRequest;

public interface TenantResolver {

    String resolveTenantCode(HttpServletRequest request);

}