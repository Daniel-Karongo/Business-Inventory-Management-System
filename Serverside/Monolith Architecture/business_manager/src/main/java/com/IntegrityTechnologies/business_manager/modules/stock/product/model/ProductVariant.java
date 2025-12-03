package com.IntegrityTechnologies.business_manager.modules.stock.product.model;

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

    private BigDecimal defaultSellingPrice;
    private String sku;
}