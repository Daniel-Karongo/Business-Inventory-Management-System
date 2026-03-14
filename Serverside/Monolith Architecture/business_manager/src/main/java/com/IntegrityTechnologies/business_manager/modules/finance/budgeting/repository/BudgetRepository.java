package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.domain.Budget;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.domain.enums.BudgetScenario;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface BudgetRepository extends JpaRepository<Budget, UUID> {

    /* ---------------------------------------------------------
       SAFE FETCH
    --------------------------------------------------------- */

    Optional<Budget> findByTenantIdAndId(
            UUID tenantId,
            UUID id
    );

    /* ---------------------------------------------------------
       GLOBAL BUDGETS
    --------------------------------------------------------- */

    List<Budget> findByTenantIdAndBranchIdIsNullAndFiscalYear(
            UUID tenantId,
            int fiscalYear
    );

    Optional<Budget> findTopByTenantIdAndBranchIdIsNullAndFiscalYearAndScenarioOrderByVersionNumberDesc(
            UUID tenantId,
            int fiscalYear,
            BudgetScenario scenario
    );

    /* ---------------------------------------------------------
       BRANCH BUDGETS
    --------------------------------------------------------- */

    List<Budget> findByTenantIdAndBranchIdAndFiscalYear(
            UUID tenantId,
            UUID branchId,
            int fiscalYear
    );

    Page<Budget> findByTenantIdAndBranchIdAndFiscalYearAndScenario(
            UUID tenantId,
            UUID branchId,
            int fiscalYear,
            BudgetScenario scenario,
            Pageable pageable
    );

    Optional<Budget> findTopByTenantIdAndBranchIdAndFiscalYearAndScenarioOrderByVersionNumberDesc(
            UUID tenantId,
            UUID branchId,
            int fiscalYear,
            BudgetScenario scenario
    );

}