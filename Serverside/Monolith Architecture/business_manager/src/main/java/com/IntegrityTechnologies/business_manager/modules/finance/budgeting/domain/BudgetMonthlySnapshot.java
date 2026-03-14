package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.domain;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "budget_monthly_snapshots",
        uniqueConstraints = {
                @UniqueConstraint(
                        columnNames = {
                                "tenant_id",
                                "branch_id",
                                "fiscalYear",
                                "monthNumber",
                                "accountId"
                        }
                )
        },
        indexes = {
                @Index(name = "idx_budgetsnapshot_tenant", columnList = "tenant_id"),
                @Index(name = "idx_budgetsnapshot_branch", columnList = "branch_id"),
                @Index(name = "idx_budgetsnapshot_year", columnList = "tenant_id,fiscalYear"),
                @Index(name = "idx_budgetsnapshot_account", columnList = "tenant_id,accountId")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class BudgetMonthlySnapshot extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false)
    private int fiscalYear;

    @Column(nullable = false)
    private int monthNumber;

    @Column(nullable = false)
    private UUID accountId;

    @Column(precision = 19, scale = 2)
    private BigDecimal planned;

    @Column(precision = 19, scale = 2)
    private BigDecimal actual;

    @Column(precision = 19, scale = 2)
    private BigDecimal variance;

    private LocalDateTime updatedAt;
}