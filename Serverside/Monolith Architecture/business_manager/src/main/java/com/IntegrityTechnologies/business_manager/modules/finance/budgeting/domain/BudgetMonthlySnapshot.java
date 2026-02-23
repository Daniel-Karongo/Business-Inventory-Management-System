package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.domain;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "budget_monthly_snapshots",
        uniqueConstraints = {
                @UniqueConstraint(
                        columnNames = {
                                "branchId",
                                "fiscalYear",
                                "monthNumber",
                                "accountId"
                        }
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BudgetMonthlySnapshot {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    private UUID branchId; // null = global consolidated

    private int fiscalYear;

    private int monthNumber;

    private UUID accountId;

    @Column(precision = 19, scale = 2)
    private BigDecimal planned;

    @Column(precision = 19, scale = 2)
    private BigDecimal actual;

    @Column(precision = 19, scale = 2)
    private BigDecimal variance;

    private LocalDateTime computedAt;
}