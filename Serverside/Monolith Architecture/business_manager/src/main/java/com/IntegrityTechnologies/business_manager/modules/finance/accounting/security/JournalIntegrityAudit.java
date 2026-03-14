package com.IntegrityTechnologies.business_manager.modules.finance.accounting.security;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "journal_integrity_audits",
        indexes = {
                @Index(
                        name = "idx_integrity_tenant_branch_verified",
                        columnList = "tenant_id,branch_id,verifiedAt"
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
public class JournalIntegrityAudit extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false)
    private LocalDateTime verifiedAt;

    @Column(nullable = false)
    private boolean valid;

    @Column(nullable = false)
    private long journalCount;

    private UUID brokenAtJournalId;

    @Column(length = 128)
    private String lastJournalHash;
}