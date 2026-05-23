package com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.enums.SupplierPaymentMethod;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.enums.SupplierPaymentStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.shared.enums.FinancialPostingStatus;
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
public class SupplierPaymentDto {

    private UUID paymentId;

    private String paymentNumber;

    private LocalDate paymentDate;

    private BigDecimal amount;

    private BigDecimal allocatedAmount;

    private BigDecimal unappliedAmount;

    private SupplierPaymentStatus status;

    private SupplierPaymentMethod paymentMethod;

    private String reference;

    private boolean posted;

    private boolean reversed;

    private FinancialPostingStatus postingStatus;

    private List<PaymentSettlementDto> allocations;
}