package com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
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
                name = "uk_recon_tenant_branch",
                columnNames = {"tenant_id","branch_id"}
        ),
        indexes = @Index(
                name = "idx_recon_tenant_branch",
                columnList = "tenant_id,branch_id"
        )
)
@Getter
@Setter
@NoArgsConstructor
public class ReconciliationState extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    private LocalDateTime lastRunAt;

    private long inconsistenciesDetected;

    private boolean autoRepairEnabled;
}