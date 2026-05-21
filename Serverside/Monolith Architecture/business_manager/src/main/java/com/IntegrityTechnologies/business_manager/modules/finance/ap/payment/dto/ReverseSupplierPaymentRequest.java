package com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Data
public class ReverseSupplierPaymentRequest {

    @NotBlank
    private String reason;
}