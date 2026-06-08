package com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.domain.OperationalExpenseSettlement;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface OperationalExpenseSettlementRepository
        extends JpaRepository<
        OperationalExpenseSettlement,
        UUID
        > {

    List<OperationalExpenseSettlement>
    findByTenantIdAndBranchIdAndExpenseId(
            UUID tenantId,
            UUID branchId,
            UUID expenseId
    );

    Optional<OperationalExpenseSettlement>
    findByTenantIdAndBranchIdAndId(
            UUID tenantId,
            UUID branchId,
            UUID id
    );

    Optional<OperationalExpenseSettlement>
    findByTenantIdAndSourceId(
            UUID tenantId,
            UUID sourceId
    );
}