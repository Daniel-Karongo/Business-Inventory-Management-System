package com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.enums.SupplierPaymentMethod;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.time.LocalDate;
import java.util.List;
import java.util.UUID;

@Data
public class BulkSupplierPaymentRequest {

    @NotNull
    private UUID branchId;

    @NotEmpty
    private List<UUID> supplierIds;

    @NotNull
    private UUID fundingAccountId;

    @NotNull
    private SupplierPaymentMethod method;

    private String reference;

    @NotNull
    private LocalDate paymentDate;

    private boolean autoAllocate = true;
}