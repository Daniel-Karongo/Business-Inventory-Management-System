package com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.enums.SupplierPaymentMethod;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.enums.SupplierPaymentStatus;
import lombok.Data;

import java.time.LocalDate;
import java.util.UUID;

@Data
public class SupplierPaymentSearchRequest {

    private UUID supplierId;

    private SupplierPaymentStatus status;

    private SupplierPaymentMethod method;

    private UUID fundingAccountId;

    private Boolean unappliedOnly;

    private Boolean reversed;

    private LocalDate fromDate;

    private LocalDate toDate;

    private String reference;
}