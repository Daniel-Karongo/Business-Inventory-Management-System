package com.IntegrityTechnologies.business_manager.common.bulk;

import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
public class BulkActionRequest {

    private List<UUID> ids;

    private String reason;
}