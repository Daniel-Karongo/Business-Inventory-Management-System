package com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountBalance;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.math.BigDecimal;
import java.util.Optional;
import java.util.UUID;

public interface AccountBalanceRepository
        extends JpaRepository<AccountBalance, UUID> {

    Optional<AccountBalance> findByAccount_IdAndBranch_Id(
            UUID accountId,
            UUID branchId
    );

    Page<AccountBalance> findByBranch_Id(
            UUID branchId,
            Pageable pageable
    );

    Page<AccountBalance> findByAccount_IdAndBranch_Id(
            UUID accountId,
            UUID branchId,
            Pageable pageable
    );
    @Modifying
    @Query(value = """
        INSERT INTO account_balances
            (id, account_id, branch_id, balance, updated_at, version)
        VALUES
            (UUID(), :accountId, :branchId, :delta, NOW(), 0)
        ON DUPLICATE KEY UPDATE
            balance = balance + :delta,
            updated_at = NOW(),
            version = version + 1
    """, nativeQuery = true)
    void applyDelta(
            @Param("accountId") UUID accountId,
            @Param("branchId") UUID branchId,
            @Param("delta") BigDecimal delta
    );

    @Modifying
    @Query(value = "DELETE FROM account_balances WHERE branch_id = :branchId", nativeQuery = true)
    void deleteBranchBalances(UUID branchId);
}