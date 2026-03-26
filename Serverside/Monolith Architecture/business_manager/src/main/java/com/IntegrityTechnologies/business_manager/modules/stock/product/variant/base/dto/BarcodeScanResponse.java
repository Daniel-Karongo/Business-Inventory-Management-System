package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto.AllocationDetail;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingAdjustment;
import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Data
@Builder
public class BarcodeScanResponse {

    // ✅ PRODUCT / VARIANT INFO (RESTORED)
    private UUID productId;
    private String productName;

    private UUID variantId;
    private String classification;
    private String sku;
    private String barcode;

    private UUID branchId;

    // ✅ SELLABLE DATA
    private UUID packagingId;

    private Long requestedQuantity;
    private Long baseUnits;

    private BigDecimal unitPrice;
    private BigDecimal totalPrice;

    private Long availableStock;

    private BigDecimal totalCost;

    private List<AllocationDetail> batchAllocations;

    private List<PricingAdjustment> adjustments;
}