package com.IntegrityTechnologies.business_manager.config.caffeine;

import java.util.UUID;

public final class CacheKeys {

    private CacheKeys(){}

    /* =========================
       USER (STRICT + SAFE)
    ========================= */

    public static String user(UUID tenantId, String identifier, Boolean deleted) {
        return tenantId + "::user::"
                + identifier + "::"
                + (deleted != null ? deleted : "ALL");
    }

    public static String usersPage(
            UUID tenantId,
            Boolean deleted,
            String role,
            UUID branchId,
            UUID departmentId,
            String q,
            int page,
            int size,
            String sort
    ) {
        return tenantId + "::users::"
                + (deleted != null ? deleted : "ALL") + "::"
                + (role != null ? role : "ALL") + "::"
                + (branchId != null ? branchId : "ALL") + "::"
                + (departmentId != null ? departmentId : "ALL") + "::"
                + (q != null ? q.trim().toLowerCase() : "") + "::"
                + page + "::"
                + size + "::"
                + (sort != null ? sort : "UNSORTED");
    }

    /* =========================
       PACKAGING
    ========================= */

    public static String packaging(UUID tenantId, UUID variantId) {
        return tenantId + "::packaging::" + variantId;
    }

    /* =========================
       PRICING
    ========================= */

    public static String pricing(
            UUID tenantId,
            UUID variantId,
            UUID packagingId,
            UUID branchId,
            UUID customerId,
            UUID groupId,
            Long quantity
    ) {
        return tenantId + "::pricing::"
                + variantId + "::"
                + packagingId + "::"
                + branchId + "::"
                + (customerId != null ? customerId : "null") + "::"
                + (groupId != null ? groupId : "null") + "::"
                + (quantity != null ? quantity : 0);
    }

    /* =========================
       VARIANT SEARCH
    ========================= */

    public static String variantSearch(
            UUID tenantId,
            UUID branchId,
            String search,
            int page,
            int size
    ) {
        String safeSearch = search == null ? "" : search.trim().toLowerCase();
        return tenantId + "::variant-search::"
                + branchId + "::"
                + safeSearch + "::"
                + page + "::"
                + size;
    }

    /* =========================
       CATEGORY
    ========================= */

    public static String categoryTree(UUID tenantId, UUID branchId) {
        return tenantId + "::category::tree::" + branchId;
    }

    public static String categoryFlat(UUID tenantId, UUID branchId) {
        return tenantId + "::category::flat::" + branchId;
    }

    public static String categorySearch(UUID tenantId, UUID branchId, String query) {
        return tenantId + "::category::search::" + branchId + "::" + query;
    }

    /* =========================
       FINANCIAL
    ========================= */

    public static String financial(UUID tenantId, UUID branchId, String type) {
        return tenantId + "::financial::" + type + "::" + branchId;
    }

    /* =========================
       BARCODE
    ========================= */

    public static String barcode(UUID tenantId, UUID branchId, String code) {
        return tenantId + "::barcode::" + branchId + "::" + code;
    }
}