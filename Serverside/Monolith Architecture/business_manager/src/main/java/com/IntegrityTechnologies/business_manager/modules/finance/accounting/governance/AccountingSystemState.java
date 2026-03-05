package com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountingMode;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "accounting_system_state",
        uniqueConstraints = @UniqueConstraint(
                name = "uk_system_state_branch",
                columnNames = "branchId"
        ),
        indexes = @Index(
                name = "idx_system_state_branch",
                columnList = "branchId"
        )
)
@Getter
@Setter
@NoArgsConstructor
public class AccountingSystemState {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false)
    private UUID branchId;

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