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
                @Index(name = "idx_snapshot_account", columnList = "accountId"),
                @Index(name = "idx_snapshot_branch_account_date", columnList = "branchId,accountId,snapshotDate")
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

    @Column(nullable = false)
    private UUID branchId;

    @Column(nullable = false)
    private UUID accountId;

    @Column(nullable = false)
    private LocalDate snapshotDate;

    /*
        Balance at start of the day
     */
    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal openingBalance;

    /*
        Total debits during the day
     */
    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal debitTotal;

    /*
        Total credits during the day
     */
    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal creditTotal;

    /*
        Balance at end of the day
     */
    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal closingBalance;

    @Column(nullable = false)
    private LocalDateTime createdAt;
}