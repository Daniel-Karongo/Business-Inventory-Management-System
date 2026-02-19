package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model;

import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import jakarta.persistence.*;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;

@Entity
@Table(
        name = "product_variants",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uq_variant_barcode",
                        columnNames = "barcode"
                ),
                @UniqueConstraint(
                        name = "uq_product_variant_classification",
                        columnNames = {"product_id", "classification"}
                )
        }
)
@Data
public class ProductVariant {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    private Product product;

    private String classification;

    @Column(nullable = false, unique = true)
    private String barcode;

    private String barcodeImagePath;

    private BigDecimal minimumSellingPrice;
    private BigDecimal averageBuyingPrice;

    @Column(nullable = false, unique = true)
    private String sku;

    @OneToMany(mappedBy = "variant", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<ProductVariantImage> images = new HashSet<>();

    @Column(nullable = false)
    private Boolean deleted = false;
}