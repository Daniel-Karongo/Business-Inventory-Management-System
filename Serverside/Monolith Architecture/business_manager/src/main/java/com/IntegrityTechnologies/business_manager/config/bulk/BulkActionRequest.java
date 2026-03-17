package com.IntegrityTechnologies.business_manager.common.bulk;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.ProductRestoreOptions;
import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
public class BulkActionRequest {

    private List<UUID> ids;
    private String reason;
    private ProductRestoreOptions restoreOptions;
}