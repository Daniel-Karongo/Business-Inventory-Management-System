package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariant;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "product_variant_packagings",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_packaging_variant_name",
                        columnNames = {"tenant_id", "branch_id", "product_variant_id", "name"}
                ),
                @UniqueConstraint(
                        name = "uk_single_base_unit",
                        columnNames = {"tenant_id", "branch_id", "product_variant_id", "is_base_unit"}
                )
        },
        indexes = {
                @Index(name = "idx_packaging_variant", columnList = "product_variant_id"),
                @Index(name = "idx_packaging_tenant_branch", columnList = "tenant_id, branch_id"),
                @Index(name = "idx_packaging_deleted", columnList = "deleted")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@EqualsAndHashCode(callSuper = true)
public class ProductVariantPackaging extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "product_variant_id", nullable = false)
    private ProductVariant productVariant;

    @Column(nullable = false)
    private String name; // e.g. "Piece", "Box", "Carton"

    @Column(nullable = false)
    private Long unitsPerPackaging; // e.g. 1, 12, 24

    @Builder.Default
    @Column(nullable = false)
    private Boolean isBaseUnit = false;

    @Builder.Default
    @Column(nullable = false)
    private Boolean deleted = false;

    private LocalDateTime deletedAt;

    @Version
    private Long version;

    @PrePersist
    @PreUpdate
    public void validate() {

        if (unitsPerPackaging == null || unitsPerPackaging <= 0) {
            throw new IllegalStateException("unitsPerPackaging must be > 0");
        }

        if (Boolean.TRUE.equals(isBaseUnit) && unitsPerPackaging != 1) {
            throw new IllegalStateException("Base unit must have unitsPerPackaging = 1");
        }
    }
}