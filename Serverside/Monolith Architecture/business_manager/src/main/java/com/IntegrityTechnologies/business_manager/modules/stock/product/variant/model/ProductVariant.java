package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model;

import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import jakarta.persistence.*;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Entity
@Table(
        name = "product_variants",
        uniqueConstraints = {
                @UniqueConstraint(name = "uq_variant_barcode", columnNames = "barcode")
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
    private List<ProductVariantImage> images = new ArrayList<>();

    @Column(nullable = false)
    private Boolean deleted = false;
}