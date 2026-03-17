package com.IntegrityTechnologies.business_manager.modules.stock.category.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.*;

@Entity
@Table(
        name = "categories",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_category_tenant_branch_parent_name",
                        columnNames = {"tenant_id", "branch_id", "parent_id", "name"}
                )
        },
        indexes = {
                @Index(name = "idx_category_tenant_branch", columnList = "tenant_id, branch_id"),
                @Index(name = "idx_category_parent", columnList = "parent_id"),
                @Index(name = "idx_category_deleted", columnList = "deleted"),
                @Index(name = "idx_category_path", columnList = "path")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class Category extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Version
    private Long version;

    @Column(nullable = false)
    private String name;

    private String description;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "parent_id")
    private Category parent;

    @OneToMany(mappedBy = "parent", cascade = CascadeType.ALL)
    private List<Category> subcategories = new ArrayList<>();

    @Column(nullable = false, length = 255)
    private String path;

    @OneToMany(mappedBy = "category")
    private List<Product> products = new ArrayList<>();

    @OneToMany(mappedBy = "category", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<CategorySupplier> categorySuppliers = new HashSet<>();

    @Column(nullable = false)
    private boolean deleted = false;

    private LocalDateTime deletedAt;
}