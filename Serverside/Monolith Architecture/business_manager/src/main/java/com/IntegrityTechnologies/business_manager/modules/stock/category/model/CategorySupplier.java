package com.IntegrityTechnologies.business_manager.modules.stock.category.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.Supplier;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;

@Entity
@Table(
        name = "category_suppliers",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_category_supplier_tenant_branch",
                        columnNames = {"tenant_id", "branch_id", "category_id", "supplier_id"}
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class CategorySupplier extends BranchAwareEntity {

    @EmbeddedId
    private CategorySupplierId id;

    @ManyToOne(fetch = FetchType.LAZY)
    @MapsId("categoryId")
    @JoinColumn(name = "category_id")
    private Category category;

    @ManyToOne(fetch = FetchType.LAZY)
    @MapsId("supplierId")
    @JoinColumn(name = "supplier_id")
    private Supplier supplier;

    private LocalDateTime createdAt;

    @PrePersist
    public void onCreate() {
        createdAt = LocalDateTime.now();
    }
}