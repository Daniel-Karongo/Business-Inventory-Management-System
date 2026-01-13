package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "product_variant_image_audits")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ProductVariantImageAudit {

    @Id
    @GeneratedValue
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