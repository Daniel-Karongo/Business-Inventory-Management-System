package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.domain;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.util.UUID;

@Entity
@Table(
        name = "budget_months",
        uniqueConstraints = {
                @UniqueConstraint(
                        columnNames = {
                                "budgetLine_id",
                                "monthNumber"
                        }
                )
        },
        indexes = {
                @Index(name = "idx_budgetmonth_tenant", columnList = "tenant_id"),
                @Index(name = "idx_budgetmonth_branch", columnList = "branch_id"),
                @Index(name = "idx_budgetmonth_line", columnList = "budgetLine_id")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BudgetMonth extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @ManyToOne(optional = false)
    @JoinColumn(name = "budgetLine_id")
    private BudgetLine budgetLine;

    @Column(nullable = false)
    private int monthNumber;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal plannedAmount;

    @PrePersist
    @PreUpdate
    private void inheritScope() {

        if (budgetLine != null) {

            setTenantId(budgetLine.getTenantId());
            setBranchId(budgetLine.getBranchId());

        }
    }
}