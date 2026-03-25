package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "product_variant_images",
        indexes = {
                @Index(name = "idx_variant_image_tenant_branch", columnList = "tenant_id, branch_id"),
                @Index(name = "idx_variant_image_variant", columnList = "variant_id")
        },
        uniqueConstraints = {
                @UniqueConstraint(
                    name = "uq_variant_image_hash",
                    columnNames = {"tenant_id", "branch_id", "content_hash"}
                )
        }
)
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public class ProductVariantImage extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "variant_id")
    private ProductVariant variant;

    @Column(nullable = false)
    private String fileName;

    @Column(nullable = false)
    private String filePath;

    private String thumbnailFileName;

    @Column(name = "content_hash", length = 64, nullable = false)
    private String contentHash;

    @Builder.Default
    @Column(nullable = false)
    private Boolean deleted = false;

    private LocalDateTime uploadedAt;

    @PrePersist
    public void onCreate() {
        if (uploadedAt == null) uploadedAt = LocalDateTime.now();
    }
}