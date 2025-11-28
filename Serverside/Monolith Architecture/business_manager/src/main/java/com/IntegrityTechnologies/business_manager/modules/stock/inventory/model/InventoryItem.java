package com.IntegrityTechnologies.business_manager.modules.stock.inventory.model;

import com.IntegrityTechnologies.business_manager.modules.person.function.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.stock.product.model.Product;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "inventory_items",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "unique_product_branch",
                        columnNames = {"product_id", "branch_id"}
                )
        },
        indexes = {
                @Index(name = "idx_inventory_product", columnList = "product_id"),
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

    /** MANY inventory items per product, one per branch */
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "product_id", nullable = false)
    private Product product;

    /** MANY inventory items per branch */
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "branch_id", nullable = false)
    private Branch branch;

    @Column(nullable = false)
    private Long quantityOnHand;

    @Column(nullable = false)
    private Long quantityReserved;

    private LocalDateTime lastUpdatedAt;

    private String lastUpdatedBy;

    /** Optimistic locking */
    @Version
    private Long version;
}