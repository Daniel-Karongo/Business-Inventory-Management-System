package com.IntegrityTechnologies.business_manager.modules.finance.sales.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
@Entity
@Table(
        name = "sale_line_items",
        indexes = {
                @Index(name = "idx_sale_line_variant", columnList = "product_variant_id"),
                @Index(name = "idx_sale_line_tenant_branch", columnList = "tenant_id, branch_id")
        }
)
@Data
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class SaleLineItem extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "product_variant_id", columnDefinition = "BINARY(16)", nullable = false)
    private UUID productVariantId;

    private String productName;

    private BigDecimal unitPrice;

    private Long quantity;

    private BigDecimal lineTotal;

    @OneToMany(
            mappedBy = "saleLineItem",
            cascade = CascadeType.ALL,
            orphanRemoval = true
    )
    @Builder.Default
    private List<SaleLineBatchSelection> batchSelections = new ArrayList<>();

    private BigDecimal netAmount;
    private BigDecimal vatAmount;
    private BigDecimal vatRate;
}