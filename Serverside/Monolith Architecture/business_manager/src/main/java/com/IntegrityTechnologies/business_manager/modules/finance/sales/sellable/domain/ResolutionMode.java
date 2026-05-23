package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.domain;

public enum ResolutionMode {
    PREVIEW,        // allocation pricing, warn on stock
    FINAL_STRICT    // allocation pricing, fail on stock
}