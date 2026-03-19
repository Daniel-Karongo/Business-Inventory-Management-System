package com.IntegrityTechnologies.business_manager.modules.stock.inventory.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "batch_reservations",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_reservation_sale_batch",
                        columnNames = {"reference_id", "batch_id"}
                )
        },
        indexes = {
                @Index(name = "idx_reservation_variant", columnList = "product_variant_id"),
                @Index(name = "idx_reservation_tenant_branch", columnList = "tenant_id, branch_id")
        }
)
@org.hibernate.annotations.Check(constraints = "quantity > 0")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BatchReservation extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false)
    private UUID batchId;

    @Column(nullable = false)
    private UUID productVariantId;

    @Column(nullable = false)
    private UUID referenceId;

    @Column(nullable = false)
    private Long quantity;

    @Column(nullable = false, updatable = false)
    private LocalDateTime createdAt;

    @PrePersist
    public void onCreate() {
        if (createdAt == null) {
            createdAt = LocalDateTime.now();
        }
    }
}