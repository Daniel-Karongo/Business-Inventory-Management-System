package com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.domain;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.UUID;

@Getter
@Setter
@Entity
@Table(
        name = "operational_expenses",
        uniqueConstraints = {
                @UniqueConstraint(
                        columnNames = {
                                "tenant_id",
                                "source_module",
                                "source_id"
                        }
                )
        }
)
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public class OperationalExpense extends BranchAwareEntity {

    @Id
    @GeneratedValue
    private UUID id;

    @Column(nullable = false)
    private UUID expenseAccountId;

    @Column(nullable = false)
    private String description;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal amount;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal settledAmount;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private ExpenseStatus status;

    @Column(nullable = false)
    private LocalDate accountingDate;

    @Column(nullable = false)
    private String sourceModule;

    @Column(nullable = false)
    private UUID sourceId;

    private UUID accrualJournalId;

    private UUID paymentJournalId;

    private boolean reversed;

    public BigDecimal getOutstandingAmount() {

        return amount.subtract(
                settledAmount == null
                        ? BigDecimal.ZERO
                        : settledAmount
        );
    }

    public void applySettlement(
            BigDecimal settlementAmount
    ) {

        if (settledAmount == null) {
            settledAmount = BigDecimal.ZERO;
        }

        settledAmount =
                settledAmount.add(
                        settlementAmount
                );

        if (
                settledAmount.compareTo(amount) >= 0
        ) {
            status = ExpenseStatus.SETTLED;
        } else {
            status = ExpenseStatus.PARTIALLY_SETTLED;
        }
    }
}