package com.IntegrityTechnologies.business_manager.modules.stock.inventory.model;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.util.UUID;

@Entity
@Table(name = "batch_consumptions")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BatchConsumption {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false)
    private UUID batchId;

    @Column(nullable = false)
    private UUID saleId;

    @Column(nullable = false)
    private UUID productVariantId;

    @Column(nullable = false)
    private Long quantity;

    @Column(nullable = false, precision = 19, scale = 6)
    private BigDecimal unitCost;
}