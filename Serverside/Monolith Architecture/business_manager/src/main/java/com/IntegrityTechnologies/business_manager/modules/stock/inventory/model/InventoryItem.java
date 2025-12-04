package com.IntegrityTechnologies.business_manager.modules.stock.inventory.model;

import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "inventory_items",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "unique_variant_branch",
                        columnNames = {"product_variant_id", "branch_id"}
                )
        },
        indexes = {
                @Index(name = "idx_inventory_product", columnList = "product_id"),
                @Index(name = "idx_inventory_variant", columnList = "product_variant_id"),
                @Index(name = "idx_inventory_branch", columnList = "branch_id")
        }
)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class InventoryItem {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(name="product_id", nullable=false)
    private UUID productId;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "product_variant_id", nullable = false)
    private ProductVariant productVariant;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "branch_id", nullable = false)
    private Branch branch;

    @Column(nullable = false)
    private Long quantityOnHand;

    @Column(nullable = false)
    private Long quantityReserved;

    @Column(precision = 18, scale = 6)
    private BigDecimal averageCost = BigDecimal.ZERO;

    private LocalDateTime lastUpdatedAt;
    private String lastUpdatedBy;

    @Version
    private Long version;
}