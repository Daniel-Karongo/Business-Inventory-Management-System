package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "budget_snapshot_state",
        uniqueConstraints = @UniqueConstraint(
                columnNames = {"branchId","fiscalYear","monthNumber"}
        ))
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BudgetSnapshotState {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    private UUID branchId;

    private int fiscalYear;
    private int monthNumber;

    private LocalDateTime computedAt;

    private boolean locked;
}