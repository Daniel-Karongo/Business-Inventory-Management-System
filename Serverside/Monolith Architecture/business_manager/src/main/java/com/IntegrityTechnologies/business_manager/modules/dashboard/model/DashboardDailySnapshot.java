package com.IntegrityTechnologies.business_manager.modules.dashboard.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
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
        name = "dashboard_daily_snapshots",
        uniqueConstraints = @UniqueConstraint(
                name = "uk_snapshot_tenant_branch_date",
                columnNames = {"tenant_id","branch_id","date"}
        ),
        indexes = {
                @Index(name = "idx_snapshot_tenant_branch_date", columnList = "tenant_id,branch_id,date")
        }
)
@Getter
@Setter
@NoArgsConstructor
public class DashboardDailySnapshot extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false)
    private LocalDate date;

    @Column(precision = 19, scale = 2)
    private BigDecimal revenue;

    @Column(precision = 19, scale = 2)
    private BigDecimal cogs;

    @Column(precision = 19, scale = 2)
    private BigDecimal profit;

    @Column(precision = 19, scale = 2)
    private BigDecimal vat;

    @Column(precision = 19, scale = 2)
    private BigDecimal cash;

    @Column(precision = 19, scale = 2)
    private BigDecimal inventory;

    private LocalDateTime computedAt;
}