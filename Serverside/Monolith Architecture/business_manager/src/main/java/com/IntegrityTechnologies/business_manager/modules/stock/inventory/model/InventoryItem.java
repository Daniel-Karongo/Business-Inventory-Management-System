package com.IntegrityTechnologies.business_manager.modules.stock.inventory.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "inventory_items",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "unique_variant_branch",
                        columnNames = {"tenant_id", "branch_id", "product_variant_id"}
                )
        },
        indexes = {
                @Index(name = "idx_inventory_tenant_branch", columnList = "tenant_id, branch_id"),
                @Index(name = "idx_inventory_variant", columnList = "product_variant_id"),
                @Index(name = "idx_inventory_deleted", columnList = "deleted")
        }
)
@Data
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class InventoryItem extends BranchAwareEntity {

    @Id
    @GeneratedValue
    private UUID id;

    @Column(name="product_id", nullable=false)
    private UUID productId;

    @Column(name = "product_variant_id", nullable = false)
    private UUID productVariantId;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "product_variant_id", insertable = false, updatable = false)
    private ProductVariant productVariant;

    @Column(nullable = false)
    private Long quantityOnHand;

    @Column(precision = 18, scale = 6)
    private BigDecimal averageCost = BigDecimal.ZERO;

    private LocalDateTime lastUpdatedAt;
    private String lastUpdatedBy;

    @Builder.Default
    @Column(nullable = false)
    private Boolean deleted = false;

    @Version
    private Long version;
}