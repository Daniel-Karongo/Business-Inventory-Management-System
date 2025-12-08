package com.IntegrityTechnologies.business_manager.modules.finance.sales.model;

import jakarta.persistence.*;
import lombok.*;
import java.math.BigDecimal;
import java.util.UUID;
@Entity
@Table(name = "sale_line_items")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SaleLineItem {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    // link to product by UUID (denormalized)
    @Column(name = "product_variant_id", columnDefinition = "BINARY(16)", nullable = false)
    private UUID productVariantId;

    private String productName;

    @Column(name = "branch_id", columnDefinition = "BINARY(16)", nullable = false)
    private UUID branchId;

    private BigDecimal unitPrice;

    private Integer quantity;

    private BigDecimal lineTotal;
}