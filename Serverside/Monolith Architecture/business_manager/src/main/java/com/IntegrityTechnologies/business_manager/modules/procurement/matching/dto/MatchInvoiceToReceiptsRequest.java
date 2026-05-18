package com.IntegrityTechnologies.business_manager.modules.procurement.matching.dto;

import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
public class MatchInvoiceToReceiptsRequest {

    @NotNull
    private UUID branchId;

    @NotNull
    private UUID purchaseInvoiceId;

    @NotEmpty
    private List<UUID> goodsReceiptIds;
}