package com.IntegrityTechnologies.business_manager.modules.procurement.matching.dto;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.ReceiveStockRequest;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Data
public class ReceiveAndInvoiceRequest {

    @Valid
    @NotNull
    private ReceiveStockRequest
            stockReceipt;
}