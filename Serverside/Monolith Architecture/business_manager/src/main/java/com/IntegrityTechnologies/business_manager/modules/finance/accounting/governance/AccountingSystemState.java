package com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountingMode;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "accounting_system_state",
        uniqueConstraints = @UniqueConstraint(
                name = "uk_system_state_tenant_branch",
                columnNames = {"tenant_id","branch_id"}
        ),
        indexes = {
                @Index(
                        name = "idx_system_state_tenant_branch",
                        columnList = "tenant_id,branch_id"
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
public class AccountingSystemState extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private AccountingMode accountingMode;

    @Column(nullable = false)
    private boolean locked = false;

    private LocalDateTime lockedAt;

    @Column(nullable = false)
    private LocalDateTime createdAt;

    @PrePersist
    public void prePersist() {
        if (createdAt == null) {
            createdAt = LocalDateTime.now();
        }
    }
}