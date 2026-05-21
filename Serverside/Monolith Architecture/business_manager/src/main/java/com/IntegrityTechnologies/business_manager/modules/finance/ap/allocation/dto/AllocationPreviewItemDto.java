package com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AllocationPreviewItemDto {

    private UUID invoiceId;

    private String invoiceNumber;

    private LocalDate invoiceDate;

    private LocalDate dueDate;

    private BigDecimal invoiceOutstanding;

    private BigDecimal allocationAmount;

    private BigDecimal remainingAfterAllocation;
}