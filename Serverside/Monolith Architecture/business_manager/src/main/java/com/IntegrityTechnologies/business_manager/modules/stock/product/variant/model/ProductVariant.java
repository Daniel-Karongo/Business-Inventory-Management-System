package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.util.*;

@Entity
@Table(
        name = "product_variants",
        uniqueConstraints = {
                @UniqueConstraint(name = "uk_variant_tenant_branch_barcode", columnNames = {"tenant_id", "branch_id", "barcode"}),
                @UniqueConstraint(name = "uk_variant_tenant_branch_sku", columnNames = {"tenant_id", "branch_id", "sku"}),
                @UniqueConstraint(
                        name = "uk_variant_product_classification",
                        columnNames = {"tenant_id", "branch_id", "product_id", "classification"}
                )
        },
        indexes = {
                @Index(name = "idx_variant_tenant_branch", columnList = "tenant_id, branch_id"),
                @Index(name = "idx_variant_deleted", columnList = "deleted"),
                @Index(name = "idx_variant_barcode_lookup", columnList = "tenant_id, branch_id, barcode"),
                @Index(name = "idx_variant_sku_lookup", columnList = "tenant_id, branch_id, sku")
        }
)
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class ProductVariant extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "product_id")
    private Product product;

    private String classification;

    @Column(nullable = false)
    private String barcode;

    private String barcodeImagePath;

    private BigDecimal minimumSellingPrice;
    private BigDecimal averageBuyingPrice;

    @Column(nullable = false)
    private String sku;

    @Builder.Default
    @OneToMany(mappedBy = "variant", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<ProductVariantImage> images = new HashSet<>();

    @Builder.Default
    @Column(nullable = false)
    private Boolean deleted = false;
}