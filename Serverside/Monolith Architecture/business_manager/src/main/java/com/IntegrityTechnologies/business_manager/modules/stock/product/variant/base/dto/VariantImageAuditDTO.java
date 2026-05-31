package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
public class VariantImageAuditDTO {

    private UUID id;

    private UUID productVariantId;

    private String productName;

    private String classification;

    private String fileName;

    private String filePath;

    private String action;

    private String reason;

    private LocalDateTime timestamp;

    private String performedBy;
}