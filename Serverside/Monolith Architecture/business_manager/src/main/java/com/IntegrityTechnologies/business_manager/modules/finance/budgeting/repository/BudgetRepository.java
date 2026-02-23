package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.domain.Budget;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.domain.enums.BudgetScenario;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface BudgetRepository extends JpaRepository<Budget, UUID> {

    List<Budget> findByFiscalYearAndScenario(
            int fiscalYear,
            BudgetScenario scenario
    );

    List<Budget> findByBranch_IdAndFiscalYearAndScenario(
            UUID branchId,
            int fiscalYear,
            BudgetScenario scenario
    );

    Optional<Budget> findTopByBranch_IdAndFiscalYearAndScenarioOrderByVersionNumberDesc(
            UUID branchId,
            int fiscalYear,
            BudgetScenario scenario
    );

    Optional<Budget> findTopByBranchIsNullAndFiscalYearAndScenarioOrderByVersionNumberDesc(
            int fiscalYear,
            BudgetScenario scenario
    );
}