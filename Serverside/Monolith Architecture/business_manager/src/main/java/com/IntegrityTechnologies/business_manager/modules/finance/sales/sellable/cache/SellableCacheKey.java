package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.cache;

import com.IntegrityTechnologies.business_manager.security.util.TenantContext;

import java.util.UUID;

public final class SellableCacheKey {

    private SellableCacheKey(){}

    public static String variants(UUID branchId, String search, int page, int size) {
        UUID tenantId = TenantContext.getTenantId();
        return tenantId + "::" + branchId + "::" + search + "::" + page + "::" + size;
    }

    public static String packaging(UUID variantId) {
        UUID tenantId = TenantContext.getTenantId();
        return tenantId + "::packaging::" + variantId;
    }

    public static String packagingPrefix(UUID variantId) {
        UUID tenantId = TenantContext.getTenantId();
        return tenantId + "::packaging::" + variantId;
    }

    public static String pricing(
            UUID variantId,
            UUID branchId,
            UUID customerId,
            UUID groupId,
            long quantity
    ) {
        UUID tenantId = TenantContext.getTenantId();
        return tenantId + "::pricing::" + variantId + "::" + branchId + "::" + customerId + "::" + groupId + "::" + quantity;
    }
}