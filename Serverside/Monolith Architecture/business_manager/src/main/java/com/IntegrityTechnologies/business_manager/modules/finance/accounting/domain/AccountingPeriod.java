package com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "accounting_periods",
        uniqueConstraints = @UniqueConstraint(
                name = "uk_branch_period",
                columnNames = {"branchId", "startDate", "endDate"}
        ),
        indexes = {
                @Index(name = "idx_period_branch_range", columnList = "branchId,startDate,endDate")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AccountingPeriod {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false)
    private UUID branchId;

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