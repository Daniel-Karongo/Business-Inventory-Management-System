package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model;

import com.IntegrityTechnologies.business_manager.modules.person.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.util.UUID;

@Entity
@Table(
        name = "product_suppliers",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_product_supplier_tenant_branch",
                        columnNames = {"tenant_id", "branch_id", "product_id", "supplier_id"}
                )
        }
)
@Getter
@Setter
@EqualsAndHashCode(
        callSuper = true,
        onlyExplicitlyIncluded = true
)
@ToString(
        exclude = {
                "product",
                "supplier"
        }
)
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class ProductSupplier extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @EqualsAndHashCode.Include
    private UUID id;

    @ManyToOne(fetch = FetchType.LAZY)
    private Product product;

    @ManyToOne(fetch = FetchType.LAZY)
    private Supplier supplier;
}