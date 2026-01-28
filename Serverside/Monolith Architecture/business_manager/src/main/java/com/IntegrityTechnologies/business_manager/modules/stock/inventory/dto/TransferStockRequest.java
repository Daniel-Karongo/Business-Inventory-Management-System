package com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto;

import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
public class TransferStockRequest {

    private UUID productVariantId;

    /** source */
    private UUID fromBranchId;

    /** destination */
    private UUID toBranchId;

    private long quantity;

    /** optional override for destination cost */
    private BigDecimal destinationUnitCost;

    private String reference;
    private String note;
}