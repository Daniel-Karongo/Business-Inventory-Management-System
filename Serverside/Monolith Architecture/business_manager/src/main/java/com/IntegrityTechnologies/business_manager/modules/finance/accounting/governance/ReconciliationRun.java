package com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "reconciliation_runs",
        indexes = {
                @Index(name = "idx_recon_run_branch", columnList = "branchId, startedAt")
        }
)
@Getter
@NoArgsConstructor
public class ReconciliationRun {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false)
    private UUID branchId;

    @Column(nullable = false)
    private LocalDateTime startedAt;

    @Column
    private LocalDateTime completedAt;

    @Column(nullable = false)
    private boolean repairEnabled;

    @Column(nullable = false)
    private long totalAccounts;

    @Column(nullable = false)
    private long inconsistencies;

    public ReconciliationRun(UUID branchId, boolean repairEnabled) {
        this.branchId = branchId;
        this.repairEnabled = repairEnabled;
        this.startedAt = LocalDateTime.now();
    }

    public void complete(long total, long inconsistencies) {
        this.totalAccounts = total;
        this.inconsistencies = inconsistencies;
        this.completedAt = LocalDateTime.now();
    }
}