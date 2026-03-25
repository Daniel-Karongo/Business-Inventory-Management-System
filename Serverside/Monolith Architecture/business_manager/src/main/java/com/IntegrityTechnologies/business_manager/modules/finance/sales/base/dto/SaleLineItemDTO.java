package com.IntegrityTechnologies.business_manager.modules.finance.sales.base.dto;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.BatchConsumptionDTO;
import lombok.Builder;
import lombok.Data;
import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Data
@Builder
public class SaleLineItemDTO {
    private UUID productVariantId;
    private String productName;
    private UUID branchId;
    private long quantity;
    private BigDecimal unitPrice;
    private BigDecimal lineTotal;
    private List<BatchConsumptionDTO> batchConsumptions;
}