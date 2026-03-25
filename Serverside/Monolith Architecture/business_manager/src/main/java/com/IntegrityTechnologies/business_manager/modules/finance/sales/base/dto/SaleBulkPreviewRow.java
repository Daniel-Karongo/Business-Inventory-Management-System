package com.IntegrityTechnologies.business_manager.modules.finance.sales.base.dto;

import java.math.BigDecimal;

public record SaleBulkPreviewRow(
        String receiptNo,
        String productName,
        String sku,
        Integer quantity,
        BigDecimal unitPrice,
        String branchCode
) {}