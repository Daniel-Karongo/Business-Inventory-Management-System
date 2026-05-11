package com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "accounting_periods",
        uniqueConstraints = @UniqueConstraint(
                name = "uk_period_tenant_branch_range",
                columnNames = {"tenant_id","branch_id","start_date","end_date"}
        ),
        indexes = {
                @Index(
                        name = "idx_period_tenant_branch_range",
                        columnList = "tenant_id,branch_id,start_date,end_date"
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class AccountingPeriod extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false)
    private LocalDate startDate;

    @Column(nullable = false)
    private LocalDate endDate;

    @Column(nullable = false)
    private boolean closed = false;

    @Column(nullable = false)
    private boolean taxAccrued = false;

    private LocalDateTime taxAccruedAt;

    private String reopenedBy;

    private LocalDateTime reopenedAt;

    private String reopenReason;

    public void close() {
        this.closed = true;
    }
}