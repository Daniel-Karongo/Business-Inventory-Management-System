package com.IntegrityTechnologies.business_manager.modules.stock.onboarding.dto;

import lombok.Data;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Data
public class StockOnboardingRequest {

    // PRODUCT
    private String productName;
    private UUID productId;

    private Long categoryId;
    private List<UUID> supplierIds;
    private Double minimumPercentageProfit;

    // VARIANT
    private String classification;
    private UUID variantId;

    // PACKAGING
    private List<PackagingInput> packagings;

    // PRICING
    private List<PricingInput> pricing;

    // INVENTORY
    private List<SupplierInput> suppliers;

    private UUID branchId;
    private String reference;
    private String note;

    @Data
    public static class PackagingInput {
        private String name;
        private Long units;
    }

    @Data
    public static class PricingInput {
        private String packagingName;
        private BigDecimal sellingPrice;
    }

    @Data
    public static class SupplierInput {
        private UUID supplierId;
        private Long unitsSupplied;
        private BigDecimal unitCost;
        private String packagingName;
    }
}