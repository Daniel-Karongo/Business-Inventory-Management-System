package com.IntegrityTechnologies.business_manager.modules.finance.accounts.dto;

import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
public class EntryLineRequest {
    private UUID accountId;
    private BigDecimal debit;   // null treated as zero
    private BigDecimal credit;  // null treated as zero
    private String note;
    private String transactionType;
}