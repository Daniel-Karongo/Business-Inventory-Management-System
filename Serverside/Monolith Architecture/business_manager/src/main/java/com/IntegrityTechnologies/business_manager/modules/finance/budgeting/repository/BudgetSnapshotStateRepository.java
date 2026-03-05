package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.domain.BudgetSnapshotState;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface BudgetSnapshotStateRepository
        extends JpaRepository<BudgetSnapshotState, UUID> {

    Optional<BudgetSnapshotState>
    findByBranchIdAndFiscalYearAndMonthNumber(
            UUID branchId,
            int fiscalYear,
            int monthNumber
    );
}