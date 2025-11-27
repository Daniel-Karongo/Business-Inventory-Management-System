package com.IntegrityTechnologies.business_manager.modules.stock.inventory.model;

import com.IntegrityTechnologies.business_manager.modules.stock.product.model.Product;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "inventory_items",
        indexes = {@Index(name = "idx_inventory_product", columnList = "product_id")})
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class InventoryItem {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @OneToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "product_id", nullable = false, unique = true)
    private Product product;

    @Column(nullable = false)
    private Long quantityOnHand;

    @Column(nullable = false)
    private Long quantityReserved;

    @Column(nullable = false)
    private String location;

    private LocalDateTime lastUpdatedAt;
    private String lastUpdatedBy;

    /** NEW — enables optimistic locking */
    @Version
    private Long version;
}