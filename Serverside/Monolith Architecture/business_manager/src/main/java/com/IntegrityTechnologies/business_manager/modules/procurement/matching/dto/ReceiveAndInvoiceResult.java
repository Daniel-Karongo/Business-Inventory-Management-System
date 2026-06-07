package com.IntegrityTechnologies.business_manager.modules.procurement.matching.dto;

import lombok.Builder;
import lombok.Data;

import java.util.List;
import java.util.Map;
import java.util.UUID;

@Data
@Builder
public class ReceiveAndInvoiceResult {

    private Map<UUID, List<UUID>> supplierInvoiceIds;
}