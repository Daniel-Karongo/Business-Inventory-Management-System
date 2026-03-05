package com.IntegrityTechnologies.business_manager.modules.finance.accounting.snapshots;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "daily_account_balance_snapshot",
        indexes = {
                @Index(name = "idx_snapshot_branch_date", columnList = "branchId,snapshotDate"),
                @Index(name = "idx_snapshot_account", columnList = "accountId")
        },
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_snapshot",
                        columnNames = {"branchId","accountId","snapshotDate"}
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
public class DailyAccountBalanceSnapshot {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    private UUID branchId;

    private UUID accountId;

    private LocalDate snapshotDate;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal balance;

    private LocalDateTime createdAt;
}