package com.IntegrityTechnologies.business_manager.modules.finance.budget.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.budget.domain.Budget;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDate;
import java.util.Optional;
import java.util.UUID;

public interface BudgetRepository extends JpaRepository<Budget, UUID> {

    Optional<Budget> findByAccountIdAndPeriodStartAndPeriodEnd(
            UUID accountId,
            LocalDate start,
            LocalDate end
    );
}