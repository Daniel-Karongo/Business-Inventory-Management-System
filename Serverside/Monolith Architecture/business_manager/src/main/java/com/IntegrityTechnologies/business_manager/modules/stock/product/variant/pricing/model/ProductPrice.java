package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.model.ProductVariantPackaging;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "product_prices",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_price_variant_packaging_qty",
                        columnNames = {
                                "tenant_id",
                                "branch_id",
                                "product_variant_id",
                                "packaging_id",
                                "min_quantity"
                        }
                )
        },
        indexes = {
                @Index(
                        name = "idx_price_lookup",
                        columnList = "tenant_id, branch_id, product_variant_id, packaging_id, min_quantity"
                ),
                @Index(name = "idx_price_deleted", columnList = "deleted")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@EqualsAndHashCode(callSuper = true)
public class ProductPrice extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    /* ============================
       RELATIONS
    ============================ */

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "product_variant_id", nullable = false)
    private ProductVariant productVariant;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "packaging_id", nullable = false)
    private ProductVariantPackaging packaging;

    /* ============================
       PRICING
    ============================ */

    @Column(nullable = false, precision = 19, scale = 6)
    private BigDecimal price;

    @Column(name = "min_quantity", nullable = false)
    private Long minQuantity; // tier threshold

    /* ============================
       CONTROL
    ============================ */

    @Builder.Default
    @Column(nullable = false)
    private Boolean deleted = false;

    private LocalDateTime deletedAt;

    @Version
    private Long version;

    @PrePersist
    public void validate() {
        if (minQuantity == null || minQuantity <= 0) {
            throw new IllegalStateException("minQuantity must be > 0");
        }
        if (price == null || price.compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalStateException("price must be >= 0");
        }
    }
}