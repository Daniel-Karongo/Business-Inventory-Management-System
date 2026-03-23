package com.IntegrityTechnologies.business_manager.modules.stock.inventory.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.hibernate.annotations.Check;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "inventory_batches",
        indexes = {
                @Index(name = "idx_batch_tenant_branch_variant", columnList = "tenant_id, branch_id, product_variant_id"),
                @Index(name = "idx_batch_fifo", columnList = "received_at")
        }
)
@Check(constraints = "quantity_remaining >= 0")
@Check(constraints = "quantity_received >= 0")
@Check(constraints = "quantity_remaining <= quantity_received")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class InventoryBatch extends BranchAwareEntity {

    @Id
    @GeneratedValue
    private UUID id;

    @Column(name = "product_variant_id", nullable = false)
    private UUID productVariantId;

    @Column(nullable = false, precision = 19, scale = 4)
    private BigDecimal unitCost;

    @Column(nullable = false)
    private Long quantityReceived;

    @Column(nullable = false)
    private Long quantityRemaining;

    @Column(nullable = false)
    private LocalDateTime receivedAt;

    @Version
    private Long version;
}