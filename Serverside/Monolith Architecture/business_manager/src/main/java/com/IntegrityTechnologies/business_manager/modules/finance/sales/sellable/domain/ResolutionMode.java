package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.domain;

public enum ResolutionMode {
    UI_FAST,        // fast pricing (avg cost), no hard stock failure
    PREVIEW,        // allocation pricing, warn on stock
    FINAL_STRICT    // allocation pricing, fail on stock
}