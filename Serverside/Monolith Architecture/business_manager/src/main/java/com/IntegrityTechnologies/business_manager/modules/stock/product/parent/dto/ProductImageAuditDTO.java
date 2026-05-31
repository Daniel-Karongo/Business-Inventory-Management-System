package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto;

import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ProductImageAuditDTO {

    private UUID id;

    private String fileName;

    private String filePath;

    private String action;

    private String reason;

    private LocalDateTime timestamp;

    private UUID productId;

    private String productName;

    private String performedBy;
}