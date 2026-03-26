package com.IntegrityTechnologies.business_manager.modules.stock.inventory.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.hibernate.annotations.Check;

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
                @Index(
                        name = "idx_reservation_active_lookup",
                        columnList = "tenant_id, branch_id, product_variant_id, status, expires_at"
                )
        }
)
@Check(constraints = "quantity > 0")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class BatchReservation extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "batch_id", nullable = false)
    private InventoryBatch batch;

    @Column(nullable = false)
    private UUID productVariantId;

    @Column(nullable = false)
    private UUID referenceId;

    @Column(nullable = false)
    private Long quantity;

    @Column(nullable = false, updatable = false)
    private LocalDateTime createdAt;

    @Column(name = "sale_id")
    private UUID saleId;

    @Column(name = "sale_line_item_id")
    private Long saleLineItemId;

    @Column(name = "packaging_id")
    private UUID packagingId;

    @Column(name = "requested_quantity")
    private Long requestedQuantity; // sell units

    @Enumerated(EnumType.STRING)
    @Column(name = "status")
    private ReservationStatus status;

    @Column(name = "expires_at")
    private LocalDateTime expiresAt;

    @Version
    private Long version;

    @PrePersist
    public void onCreate() {
        if (createdAt == null) {
            createdAt = LocalDateTime.now();
        }
    }
}