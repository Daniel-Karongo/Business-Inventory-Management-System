package com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.dto.PaymentSettlementDto;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class SupplierPaymentDetailsResponse {

    private SupplierPaymentResponse payment;

    private List<PaymentSettlementDto> allocations;
}