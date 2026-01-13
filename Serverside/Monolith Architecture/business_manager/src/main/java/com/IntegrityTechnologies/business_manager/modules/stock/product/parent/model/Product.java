package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model;

import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import jakarta.persistence.*;
import lombok.*;

import org.hibernate.annotations.JdbcTypeCode;

import java.sql.Types;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;

@Entity
@Table(
        name = "products",
        indexes = {
                @Index(name = "idx_product_name", columnList = "name"),
                @Index(name = "idx_product_sku", columnList = "sku")
        }
)
@Data
@NoArgsConstructor
@AllArgsConstructor
public class Product {

    @Id
    @GeneratedValue
    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    /* =============================
       CORE FIELDS
       ============================= */

    @Column(nullable = false, unique = true)
    private String name;

    private String description;

    @Column(unique = true)
    private String sku;

    /**
     * NOTE:
     * - barcode and barcodeImagePath REMOVED
     * - barcodes are now owned exclusively by ProductVariant
     */

    private Double minimumPercentageProfit;

    /* =============================
       VARIANTS
       ============================= */

    @OneToMany(
            mappedBy = "product",
            cascade = CascadeType.ALL,
            orphanRemoval = true
    )
    private List<ProductVariant> variants = new ArrayList<>();

    /* =============================
       SUPPLIERS  (IMPORTANT)
       ============================= */

    /**
     * Suppliers remain linked to the PRODUCT (not variants).
     * This preserves:
     * - purchasing workflows
     * - category–supplier syncing
     * - reporting consistency
     */
    @ManyToMany(fetch = FetchType.LAZY)
    @JoinTable(
            name = "products_suppliers",
            joinColumns = @JoinColumn(name = "product_id"),
            inverseJoinColumns = @JoinColumn(name = "supplier_id")
    )
    private Set<Supplier> suppliers = new HashSet<>();

    /* =============================
       PRODUCT IMAGES (STILL VALID)
       ============================= */

    @OneToMany(
            mappedBy = "product",
            cascade = CascadeType.ALL,
            orphanRemoval = true
    )
    private List<ProductImage> images = new ArrayList<>();

    /* =============================
       CATEGORY
       ============================= */

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "category_id")
    private Category category;

    /* =============================
       SOFT DELETE + AUDIT
       ============================= */

    private Boolean deleted;
    private LocalDateTime deletedAt;

    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    @PrePersist
    public void onCreate() {
        createdAt = LocalDateTime.now();
        deleted = (deleted == null) ? false : deleted;
    }

    @PreUpdate
    public void onUpdate() {
        updatedAt = LocalDateTime.now();
    }
}