package com.IntegrityTechnologies.business_manager.modules.stock.inventory.model;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "inventory_batches",
        indexes = {
                @Index(name = "idx_batch_variant_branch",
                        columnList = "productVariantId, branchId"),
                @Index(name = "idx_batch_fifo",
                        columnList = "receivedAt")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class InventoryBatch {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false, columnDefinition = "BINARY(16)")
    private UUID productVariantId;

    @Column(nullable = false, columnDefinition = "BINARY(16)")
    private UUID branchId;

    @Column(nullable = false, precision = 19, scale = 4)
    private BigDecimal unitCost;

    @Column(nullable = false, precision = 19, scale = 6)
    private BigDecimal unitSellingPrice;

    @Column(nullable = false)
    private Long quantityReceived;

    @Column(nullable = false)
    private Long quantityRemaining;

    @Column(nullable = false)
    private LocalDateTime receivedAt;

    @Version
    private Long version;
}