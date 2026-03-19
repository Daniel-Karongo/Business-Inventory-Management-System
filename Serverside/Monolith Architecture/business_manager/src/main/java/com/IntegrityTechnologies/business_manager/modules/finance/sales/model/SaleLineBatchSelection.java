package com.IntegrityTechnologies.business_manager.modules.finance.sales.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.util.UUID;

@Entity
@Table(
        name = "sale_line_batch_selections",
        indexes = {
                @Index(name = "idx_batch_selection_batch", columnList = "batch_id"),
                @Index(name = "idx_batch_selection_scope", columnList = "tenant_id, branch_id")
        }
)
@Data
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class SaleLineBatchSelection extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "batch_id", columnDefinition = "BINARY(16)", nullable = false)
    private UUID batchId;

    @Column(nullable = false)
    private Long quantity;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "sale_line_item_id", nullable = false)
    private SaleLineItem saleLineItem;
}