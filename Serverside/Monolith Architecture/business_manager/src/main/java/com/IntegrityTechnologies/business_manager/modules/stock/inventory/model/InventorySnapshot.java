package com.IntegrityTechnologies.business_manager.modules.stock.inventory.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.hibernate.annotations.UuidGenerator;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "inventory_snapshots",
        uniqueConstraints = @UniqueConstraint(
                columnNames = {"tenant_id", "branch_id", "product_id", "product_variant_id", "snapshot_date"}
        )
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class InventorySnapshot extends BranchAwareEntity {

    @Id
    @UuidGenerator
    private UUID id;

    private UUID productId;
    private UUID productVariantId;

    private Long quantityOnHand;

    private BigDecimal valuation;

    private LocalDate snapshotDate;
    private LocalDateTime createdAt;
}