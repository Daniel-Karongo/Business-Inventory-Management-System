package com.IntegrityTechnologies.business_manager.modules.stock.onboarding.dto;

import lombok.Data;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Data
public class StockOnboardingRequest {

    /* =====================================================
       PRODUCT
    ===================================================== */

    private String productName;

    private UUID productId;

    private Long categoryId;

    /**
     * Used when category does not exist.
     */
    private String newCategoryName;

    /**
     * Optional explicit control.
     * Defaults handled in service.
     */
    private Boolean createCategoryIfMissing;

    /**
     * Future-safe import behavior.
     */
    private Boolean createProductIfMissing;

    private List<UUID> supplierIds;

    private Double minimumPercentageProfit;

    /* =====================================================
       VARIANT
    ===================================================== */

    private String classification;

    private UUID variantId;

    /* =====================================================
       PACKAGING
    ===================================================== */

    private List<PackagingInput> packagings;

    /* =====================================================
       PRICING
    ===================================================== */

    private List<PricingInput> pricing;

    /* =====================================================
       INVENTORY
    ===================================================== */

    private List<SupplierInput> suppliers;

    private UUID branchId;

    private String reference;

    private String note;

    /* =====================================================
       PACKAGING INPUT
    ===================================================== */

    @Data
    public static class PackagingInput {

        private String name;

        private Long units;
    }

    /* =====================================================
       PRICING INPUT
    ===================================================== */

    @Data
    public static class PricingInput {

        private String packagingName;

        private BigDecimal sellingPrice;
    }

    /* =====================================================
       SUPPLIER INPUT
    ===================================================== */

    @Data
    public static class SupplierInput {

        /**
         * Existing supplier flow.
         */
        private UUID supplierId;

        /**
         * Inline supplier creation flow.
         */
        private String supplierName;

        /**
         * Explicit create behavior.
         */
        private Boolean createSupplierIfMissing;

        private Long unitsSupplied;

        /**
         * Cost per supplied packaging.
         */
        private BigDecimal unitCost;

        /**
         * Packaging used during intake.
         */
        private String packagingName;
    }
}