package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class WarningDTO {
    private String type;     // LOW_STOCK, OUT_OF_STOCK, BELOW_MIN_PRICE
    private String message;
}