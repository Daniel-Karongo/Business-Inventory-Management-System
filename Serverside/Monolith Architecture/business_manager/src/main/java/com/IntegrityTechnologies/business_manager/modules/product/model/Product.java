package com.IntegrityTechnologies.business_manager.modules.product.model;

import com.IntegrityTechnologies.business_manager.modules.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.Supplier; // optional
import com.IntegrityTechnologies.business_manager.modules.user.model.User;
import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "products")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Product {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

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

    private boolean deleted = false;

    private LocalDateTime deletedAt;

    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "category_id")
    private Category category;

    // Supplier is optional — mapping will be lazy and can be null until supplier module exists
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "supplier_id")
    private Supplier supplier;

    // Who last supplied this product
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "last_supplied_by_id")
    private User lastSuppliedBy;

    @OneToMany(mappedBy = "product", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<ProductImage> images;
}