package com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountBalance;
import org.springframework.data.domain.*;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.Optional;
import java.util.UUID;

@Transactional
public interface AccountBalanceRepository
        extends JpaRepository<AccountBalance, AccountBalance.AccountBalanceId> {

    Optional<AccountBalance> findByTenantIdAndAccount_IdAndBranch_Id(
            UUID tenantId,
            UUID accountId,
            UUID branchId
    );

    Page<AccountBalance> findByTenantIdAndBranch_Id(
            UUID tenantId,
            UUID branchId,
            Pageable pageable
    );

    Page<AccountBalance> findByTenantIdAndAccount_IdAndBranch_Id(
            UUID tenantId,
            UUID accountId,
            UUID branchId,
            Pageable pageable
    );

    @Modifying
    @Query(value = """
        INSERT INTO account_balances
           (tenant_id, account_id, branch_id, balance, updated_at, version)
        VALUES
           (:tenantId, :accountId, :branchId, :delta, NOW(), 0)
        ON DUPLICATE KEY UPDATE
           balance = balance + :delta,
           updated_at = NOW(),
           version = version + 1
    """, nativeQuery = true)
    void applyDelta(
            @Param("tenantId") UUID tenantId,
            @Param("accountId") UUID accountId,
            @Param("branchId") UUID branchId,
            @Param("delta") BigDecimal delta
    );

    @Modifying
    @Query(value = """
        DELETE FROM account_balances
        WHERE tenant_id = :tenantId
          AND branch_id = :branchId
    """, nativeQuery = true)
    void deleteBranchBalances(
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );
}