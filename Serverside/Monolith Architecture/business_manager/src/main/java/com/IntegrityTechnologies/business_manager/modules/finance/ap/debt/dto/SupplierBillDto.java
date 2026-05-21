package com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SupplierBillDto {

    private UUID invoiceId;

    private String billNumber;

    private LocalDate invoiceDate;
    private LocalDate dueDate;

    private BigDecimal totalAmount;
    private BigDecimal paidAmount;
    private BigDecimal remainingAmount;

    private boolean overdue;

    private String status;

    private List<SupplierBillLineDto> items;

    private List<InvoiceSettlementDto> settlements;
}