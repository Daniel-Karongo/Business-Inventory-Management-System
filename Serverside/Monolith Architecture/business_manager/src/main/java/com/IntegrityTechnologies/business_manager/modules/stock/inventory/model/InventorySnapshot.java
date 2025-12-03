package com.IntegrityTechnologies.business_manager.modules.stock.inventory.model;

import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.*;
import org.hibernate.annotations.UuidGenerator;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "inventory_snapshots",
        uniqueConstraints = @UniqueConstraint(
                columnNames = {"product_id", "branch_id", "snapshot_date"}
        )
)
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class InventorySnapshot {

    @Id
    @UuidGenerator
    private UUID id;

    private UUID productId;
    private UUID branchId;

    private Long quantityOnHand;
    private Long quantityReserved;

    private BigDecimal valuation;

    private LocalDate snapshotDate;
    private LocalDateTime createdAt;
}