package com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.util.UUID;

@Entity
@Table(
        name = "reconciliation_items",
        indexes = {
                @Index(name = "idx_recon_item_run", columnList = "run_id"),
                @Index(name = "idx_recon_item_account", columnList = "accountId")
        }
)
@Getter
@NoArgsConstructor
public class ReconciliationItem {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false)
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
            UUID accountId,
            BigDecimal ledgerBalance,
            BigDecimal projectedBalance,
            boolean consistent,
            ReconciliationRun run
    ) {
        this.accountId = accountId;
        this.ledgerBalance = ledgerBalance;
        this.projectedBalance = projectedBalance;
        this.consistent = consistent;
        this.run = run;
    }
}