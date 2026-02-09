package com.IntegrityTechnologies.business_manager.modules.finance.sales.dto;

import lombok.Data;

import java.math.BigDecimal;

/**
 * One row = one SALE LINE ITEM.
 * Rows are grouped into sales using receiptNo (client-side only).
 */
@Data
public class SaleBulkRow {

    /** Client-side grouping key (NOT persisted) */
    private String receiptNo;

    /** Product identification (either SKU or name) */
    private String sku;
    private String productName;

    /** Optional variant classification, default = STANDARD */
    private String variant;

    /** Line item */
    private Integer quantity;
    private BigDecimal unitPrice;

    /** Sale-level fields (resolved once per receiptNo) */
    private String branchCode; // default MAIN
    private String saleDate;   // ISO string, optional
    private String payments;   // appears once per receiptNo
}