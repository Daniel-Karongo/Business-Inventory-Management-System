package com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.domain;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.UUID;

@Getter
@Setter
@Entity
@Table(
        name = "operational_expense_settlements",
        uniqueConstraints = {
                @UniqueConstraint(
                        columnNames = {
                                "tenant_id",
                                "source_id"
                        }
                )
        }
)
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public class OperationalExpenseSettlement extends BranchAwareEntity {

    @Id
    @GeneratedValue
    private UUID id;

    @Column(nullable = false)
    private UUID expenseId;

    @Column(nullable = false)
    private UUID fundingAccountId;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal amount;

    @Column(nullable = false)
    private LocalDate settlementDate;

    private String reference;

    @Column(nullable = false)
    private UUID sourceId;

    private UUID settlementJournalId;

    private boolean reversed;
}