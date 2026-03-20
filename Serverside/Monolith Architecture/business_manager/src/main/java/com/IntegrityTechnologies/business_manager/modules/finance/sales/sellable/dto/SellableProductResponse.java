package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto;

import com.IntegrityTechnologies.business_manager.config.response.PageWrapper;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class SellableProductResponse {
    private PageWrapper<SellableVariantDTO> variants;
}