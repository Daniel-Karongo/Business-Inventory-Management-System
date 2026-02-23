package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.domain;

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
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BudgetMonth {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @ManyToOne(optional = false)
    @JoinColumn(name = "budgetLine_id")
    private BudgetLine budgetLine;

    @Column(nullable = false)
    private int monthNumber; // 1–12

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal plannedAmount;
}