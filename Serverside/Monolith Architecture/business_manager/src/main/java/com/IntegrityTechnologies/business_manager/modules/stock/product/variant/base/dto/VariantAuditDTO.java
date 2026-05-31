package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
public class VariantAuditDTO {

    private UUID id;

    private String action;

    private String fieldChanged;

    private String oldValue;

    private String newValue;

    private String reason;

    private LocalDateTime timestamp;

    private UUID productId;

    private String productName;

    private UUID variantId;

    private String classification;

    private String performedBy;
}