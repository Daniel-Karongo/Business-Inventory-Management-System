package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.domain;

import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.domain.enums.BudgetScenario;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.*;

@Entity
@Table(
        name = "budgets",
        uniqueConstraints = {
                @UniqueConstraint(
                        columnNames = {
                                "branch_id",
                                "fiscalYear",
                                "versionNumber",
                                "scenario"
                        }
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Budget {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    /**
     * NULL branch = GLOBAL BUDGET
     */
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "branch_id")
    private Branch branch;

    @Column(nullable = false)
    private int fiscalYear;

    @Column(nullable = false)
    private int versionNumber;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private BudgetScenario scenario;

    @Column(nullable = false)
    private boolean locked = false;

    @Column(nullable = false, updatable = false)
    private LocalDateTime createdAt;

    @Column(nullable = false, updatable = false)
    private String createdBy;

    @OneToMany(mappedBy = "budget",
            cascade = CascadeType.ALL,
            orphanRemoval = true)
    @Builder.Default
    private List<BudgetLine> lines = new ArrayList<>();

    @PrePersist
    public void onCreate() {
        this.createdAt = LocalDateTime.now();
    }

    public boolean isGlobal() {
        return branch == null;
    }

    public void lock() {
        this.locked = true;
    }

    public void ensureEditable() {
        if (locked) {
            throw new IllegalStateException("Budget is locked and cannot be modified");
        }
    }
}