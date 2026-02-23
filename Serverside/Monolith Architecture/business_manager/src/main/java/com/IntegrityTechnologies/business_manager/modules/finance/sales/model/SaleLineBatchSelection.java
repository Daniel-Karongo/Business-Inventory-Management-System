package com.IntegrityTechnologies.business_manager.modules.finance.sales.model;

import jakarta.persistence.*;
import lombok.*;

import java.util.UUID;

@Entity
@Table(name = "sale_line_batch_selections")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SaleLineBatchSelection {

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