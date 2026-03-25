package com.IntegrityTechnologies.business_manager.modules.stock.category.cache;

import com.IntegrityTechnologies.business_manager.security.util.BranchContext;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;

import java.util.UUID;

public final class CategoryCacheKey {

    private CategoryCacheKey(){}

    public static String tree(UUID branchId, Boolean deleted) {
        return base(branchId,"tree:" + deleted);
    }

    public static String flat(UUID branchId, Boolean deleted) {
        return base(branchId,"flat:" + deleted);
    }

    public static String search(UUID branchId, String keyword, boolean deleted) {
        return base(branchId,"search:" + keyword + ":" + deleted);
    }

    private static String base(UUID branchId, String suffix) {
        return TenantContext.getTenantId() + "::" +
                branchId + "::category::" + suffix;
    }
}
