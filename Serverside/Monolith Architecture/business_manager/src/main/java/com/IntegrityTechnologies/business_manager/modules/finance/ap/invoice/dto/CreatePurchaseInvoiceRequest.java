package com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.dto;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.time.LocalDate;
import java.util.List;
import java.util.UUID;

@Data
public class CreatePurchaseInvoiceRequest {

    @NotNull
    private UUID branchId;

    @NotNull
    private UUID supplierId;

    @NotBlank
    private String supplierInvoiceNumber;

    @NotNull
    private LocalDate invoiceDate;

    @NotNull
    private LocalDate dueDate;

    private String notes;

    @Valid
    @NotEmpty
    private List<CreatePurchaseInvoiceLineRequest> lines;
}