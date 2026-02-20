package com.IntegrityTechnologies.business_manager.modules.finance.budget.domain;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.UUID;

@Entity
@Table(name = "budgets",
        uniqueConstraints = @UniqueConstraint(
                columnNames = {"account_id", "period_start", "period_end"}
        ))
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Budget {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(name = "account_id", nullable = false)
    private UUID accountId;

    private LocalDate periodStart;
    private LocalDate periodEnd;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal plannedAmount;
}