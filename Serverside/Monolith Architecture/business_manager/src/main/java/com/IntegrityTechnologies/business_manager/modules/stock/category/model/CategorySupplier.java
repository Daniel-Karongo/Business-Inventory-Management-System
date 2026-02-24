package com.IntegrityTechnologies.business_manager.modules.stock.category.model;

import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.Supplier;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "category_suppliers",
        uniqueConstraints = {
                @UniqueConstraint(columnNames = {"category_id", "supplier_id"})
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CategorySupplier {

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