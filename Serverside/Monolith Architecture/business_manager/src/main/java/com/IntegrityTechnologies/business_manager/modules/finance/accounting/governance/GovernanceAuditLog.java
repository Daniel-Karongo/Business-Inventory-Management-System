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
        name = "governance_audit_log",
        indexes = {
                @Index(
                        name = "idx_gov_tenant_branch_action_time",
                        columnList = "tenant_id,branch_id,performedAt"
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
public class GovernanceAuditLog extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false)
    private String action;

    @Column(nullable = false)
    private String performedBy;

    @Column(nullable = false)
    private LocalDateTime performedAt;

    @Column(length = 1000)
    private String details;
}