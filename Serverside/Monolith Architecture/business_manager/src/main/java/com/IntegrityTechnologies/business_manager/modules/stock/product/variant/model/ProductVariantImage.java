package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "product_variant_images",
        indexes = {
                @Index(name = "idx_variant_image_variant", columnList = "variant_id")
        }
)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ProductVariantImage {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    /* =============================
       RELATION (MUST MATCH DB)
       ============================= */

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(
            name = "variant_id",
            nullable = false
    )
    private ProductVariant variant;

    /* =============================
       FILE METADATA
       ============================= */

    @Column(nullable = false)
    private String fileName;

    @Column(nullable = false)
    private String filePath;

    @Column(nullable = false)
    private Boolean deleted = false;

    private LocalDateTime uploadedAt;

    @PrePersist
    public void onCreate() {
        if (uploadedAt == null)
            uploadedAt = LocalDateTime.now();
        if (deleted == null)
            deleted = false;
    }
}