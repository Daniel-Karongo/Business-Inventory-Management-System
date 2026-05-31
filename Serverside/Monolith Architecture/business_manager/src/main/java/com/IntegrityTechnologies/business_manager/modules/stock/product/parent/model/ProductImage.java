package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "product_images",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uq_product_image_hash",
                        columnNames = {"tenant_id", "branch_id", "content_hash"}
                )
        },
        indexes = {
                @Index(name = "idx_product_image_tenant_branch", columnList = "tenant_id, branch_id"),
                @Index(name = "idx_product_image_product", columnList = "product_id")
        }
)
@Getter
@Setter
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(
        callSuper = true,
        onlyExplicitlyIncluded = true
)
@ToString(
        exclude = {
                "product"
        }
)
public class ProductImage extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @EqualsAndHashCode.Include
    private UUID id;

    private String fileName;
    private String filePath;
    private String thumbnailFileName;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "product_id")
    private Product product;

    @Column(name = "content_hash", length = 64, nullable = false)
    private String contentHash;

    @Builder.Default
    @Column(nullable = false)
    private Boolean primaryImage = false;

    @Builder.Default
    @Column(nullable = false)
    private Boolean deletedIndependently = false;

    private LocalDateTime uploadedAt;

    @Override
    public void beforePersist() {
        if (uploadedAt == null) uploadedAt = LocalDateTime.now();
    }
}