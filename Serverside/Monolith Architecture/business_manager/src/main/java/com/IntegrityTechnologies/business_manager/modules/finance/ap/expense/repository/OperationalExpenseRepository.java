package com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.domain.ExpenseStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.domain.OperationalExpense;
import jakarta.persistence.LockModeType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface OperationalExpenseRepository
        extends JpaRepository<OperationalExpense, UUID> {

    List<OperationalExpense> findByTenantIdAndBranchIdAndStatusIn(
            UUID tenantId,
            UUID branchId,
            List<ExpenseStatus> statuses
    );

    Optional<OperationalExpense> findByTenantIdAndBranchIdAndId(
            UUID tenantId,
            UUID branchId,
            UUID id
    );

    List<OperationalExpense>
    findByTenantIdAndBranchIdAndStatus(
            UUID tenantId,
            UUID branchId,
            ExpenseStatus status
    );

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("""
            select e
            from OperationalExpense e
            where e.tenantId = :tenantId
            and e.branchId = :branchId
            and e.id = :id
            """)
    Optional<OperationalExpense> findForUpdate(
            UUID tenantId,
            UUID branchId,
            UUID id
    );

    Page<OperationalExpense> findByTenantIdAndBranchId(
            UUID tenantId,
            UUID branchId,
            Pageable pageable
    );

    Page<OperationalExpense> findByTenantIdAndBranchIdAndStatus(
            UUID tenantId,
            UUID branchId,
            ExpenseStatus status,
            Pageable pageable
    );

    @Query("""
            select e
            from OperationalExpense e
            where e.tenantId = :tenantId
            and e.branchId = :branchId
            and (
                 lower(e.description) like lower(concat('%', :search, '%'))
            )
            """)
    Page<OperationalExpense> search(
            UUID tenantId,
            UUID branchId,
            String search,
            Pageable pageable
    );
}