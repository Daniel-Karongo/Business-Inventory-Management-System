package com.IntegrityTechnologies.business_manager.modules.product.model;

import com.IntegrityTechnologies.business_manager.modules.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.Supplier;
import jakarta.persistence.*;
import lombok.*;

import org.hibernate.annotations.JdbcTypeCode;

import java.math.BigDecimal;
import java.sql.Types;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;

@Entity
@Table(name = "products",
        indexes = {
                @Index(name = "idx_product_name", columnList = "name"),
                @Index(name = "idx_barcode", columnList = "barcode")
        })
@Data
@NoArgsConstructor
@AllArgsConstructor
public class Product {

    @Id
    @GeneratedValue
    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(nullable = false, unique = true)
    private String name;

    private String description;

    @Column(unique = true)
    private String sku;

    @Column(unique = true)
    private String barcode;

    private String barcodeImagePath;

    @Column(nullable = false)
    private BigDecimal price;

    private BigDecimal buyingPrice;

    private Integer stockQuantity;

    /**
     * Manual soft delete flag. We are not using @SQLDelete/@Where here (per your choice).
     */
    private Boolean deleted;

    private LocalDateTime deletedAt;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "category_id")
    private Category category;

    @ManyToMany(fetch = FetchType.LAZY)
    @JoinTable(
            name = "products_suppliers",
            joinColumns = @JoinColumn(name = "product_id"),
            inverseJoinColumns = @JoinColumn(name = "supplier_id")
    )
    private Set<Supplier> suppliers = new HashSet<>();

    // Who last supplied this product
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "last_supplied_by_id")
    private Supplier lastSuppliedBy;

    @OneToMany(mappedBy = "product", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<ProductImage> images = new ArrayList<>();

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