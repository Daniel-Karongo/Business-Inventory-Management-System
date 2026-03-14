package com.IntegrityTechnologies.business_manager.modules.finance.accounting.snapshots;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "daily_account_balance_snapshot",
        indexes = {

                @Index(
                        name = "idx_snapshot_tenant_branch_date",
                        columnList = "tenant_id,branch_id,snapshotDate"
                ),

                @Index(
                        name = "idx_snapshot_tenant_account",
                        columnList = "tenant_id,accountId"
                ),

                @Index(
                        name = "idx_snapshot_tenant_branch_account_date",
                        columnList = "tenant_id,branch_id,accountId,snapshotDate"
                )
        },
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_snapshot",
                        columnNames = {"tenant_id","branch_id","accountId","snapshotDate"}
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
public class DailyAccountBalanceSnapshot extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false)
    private UUID accountId;

    @Column(nullable = false)
    private LocalDate snapshotDate;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal openingBalance;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal debitTotal;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal creditTotal;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal closingBalance;

    @Column(nullable = false)
    private LocalDateTime createdAt;
}