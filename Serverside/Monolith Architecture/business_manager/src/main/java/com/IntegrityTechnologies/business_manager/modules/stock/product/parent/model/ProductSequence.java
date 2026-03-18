package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

@Entity
@Table(
        name = "product_sequences",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_sequence_tenant_branch_category",
                        columnNames = {"tenant_id", "branch_id", "category_id"}
                )
        },
        indexes = {
                @Index(name = "idx_sequence_tenant_branch", columnList = "tenant_id, branch_id")
        }
)
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public class ProductSequence extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "category_id", nullable = false)
    private Long categoryId;

    @Column(name = "last_seq_value", nullable = false)
    private Long lastValue = 0L;

    @Version
    private Long version;
}