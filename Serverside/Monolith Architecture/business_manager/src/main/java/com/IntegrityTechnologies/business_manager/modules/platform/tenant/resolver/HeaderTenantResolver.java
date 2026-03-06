package com.IntegrityTechnologies.business_manager.modules.platform.tenant.resolver;

import jakarta.servlet.http.HttpServletRequest;
import org.springframework.stereotype.Component;

@Component
public class HeaderTenantResolver implements TenantResolver {

    private static final String TENANT_HEADER = "X-Tenant-Code";

    @Override
    public String resolveTenantCode(HttpServletRequest request) {

        return request.getHeader(TENANT_HEADER);

    }
}