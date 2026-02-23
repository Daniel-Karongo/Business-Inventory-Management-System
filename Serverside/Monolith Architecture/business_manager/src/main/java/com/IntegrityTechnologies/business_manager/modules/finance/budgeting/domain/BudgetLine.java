package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.domain;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.Account;
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
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BudgetLine {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @ManyToOne(optional = false)
    @JoinColumn(name = "budget_id")
    private Budget budget;

    @ManyToOne(optional = false)
    @JoinColumn(name = "account_id")
    private Account account;

    @OneToMany(mappedBy = "budgetLine",
            cascade = CascadeType.ALL,
            orphanRemoval = true)
    @Builder.Default
    private List<BudgetMonth> months = new ArrayList<>();
}