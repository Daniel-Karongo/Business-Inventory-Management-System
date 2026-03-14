package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.domain;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.Account;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;

import java.util.*;

@Entity
@Table(
        name = "budget_lines",
        uniqueConstraints = {
                @UniqueConstraint(
                        columnNames = {
                                "budget_id",
                                "account_id"
                        }
                )
        },
        indexes = {
                @Index(name = "idx_budgetline_tenant", columnList = "tenant_id"),
                @Index(name = "idx_budgetline_branch", columnList = "branch_id"),
                @Index(name = "idx_budgetline_budget", columnList = "budget_id")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BudgetLine extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @ManyToOne(optional = false)
    @JoinColumn(name = "budget_id")
    private Budget budget;

    @ManyToOne(optional = false)
    @JoinColumn(name = "account_id")
    private Account account;

    @OneToMany(
            mappedBy = "budgetLine",
            cascade = CascadeType.ALL,
            orphanRemoval = true
    )
    @Builder.Default
    private List<BudgetMonth> months = new ArrayList<>();

    @PrePersist
    @PreUpdate
    private void inheritScope() {

        if (budget != null) {
            setTenantId(budget.getTenantId());
            setBranchId(budget.getBranchId());
        }
    }
}