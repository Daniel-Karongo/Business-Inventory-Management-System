package com.IntegrityTechnologies.business_manager.modules.finance.ap.reporting.dto;

import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SupplierStatementEntryDto {

    private LocalDateTime transactionDate;

    private String branchName;

    private String documentType;

    private UUID documentId;

    private String documentNumber;

    private String referenceNumber;

    private String description;

    private BigDecimal debitAmount;

    private BigDecimal creditAmount;

    private BigDecimal runningBalance;
}