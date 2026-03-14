package com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.util.UUID;

@Entity
@Table(
        name = "reconciliation_items",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_recon_run_account",
                        columnNames = {"run_id", "account_id"}
                )
        },
        indexes = {
                @Index(name = "idx_recon_item_run", columnList = "run_id"),
                @Index(name = "idx_recon_item_account", columnList = "account_id")
        }
)
@Getter
@NoArgsConstructor
public class ReconciliationItem extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(name = "account_id", nullable = false)
    private UUID accountId;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal ledgerBalance;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal projectedBalance;

    @Column(nullable = false)
    private boolean consistent;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "run_id", nullable = false)
    private ReconciliationRun run;

    public ReconciliationItem(
            UUID tenantId,
            UUID branchId,
            UUID accountId,
            BigDecimal ledgerBalance,
            BigDecimal projectedBalance,
            boolean consistent,
            ReconciliationRun run
    ) {

        this.setTenantId(tenantId);
        this.setBranchId(branchId);

        this.accountId = accountId;
        this.ledgerBalance = ledgerBalance;
        this.projectedBalance = projectedBalance;
        this.consistent = consistent;
        this.run = run;
    }

    public UUID getRunId() {
        return run.getId();
    }
}