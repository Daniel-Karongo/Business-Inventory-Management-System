package com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "accounting_reconciliation_state",
        uniqueConstraints = @UniqueConstraint(
                name = "uk_recon_branch",
                columnNames = "branchId"
        ),
        indexes = @Index(
                name = "idx_recon_branch",
                columnList = "branchId"
        )
)
@Getter
@Setter
@NoArgsConstructor
public class ReconciliationState {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false)
    private UUID branchId;

    private LocalDateTime lastRunAt;

    private long inconsistenciesDetected;

    private boolean autoRepairEnabled;
}