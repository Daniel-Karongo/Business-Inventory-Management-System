package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model;

import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import jakarta.persistence.*;
import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Entity
@Table(name = "product_variants")
@Data
public class ProductVariant {

    @Id
    @GeneratedValue
    private UUID id;

    @ManyToOne
    private Product product;

    private String classification;   // "ORIGINAL", "GENERIC", "GRADE A", etc.

    private BigDecimal minimumSellingPrice;
    private BigDecimal averageBuyingPrice;
    private String sku;
}