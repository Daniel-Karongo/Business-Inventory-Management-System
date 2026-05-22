package com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.Account;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountRole;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.projection.AccountListItemProjection;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface AccountRepository extends JpaRepository<Account, UUID> {

    @Query("""
                SELECT
                    a.id AS id,
                    a.code AS code,
                    a.name AS name,
                    a.type AS type,
                    a.role AS role,
                    a.active AS active,
                    COALESCE(ab.balance, 0) AS balance,
                    ab.updatedAt AS updatedAt
                FROM Account a
                LEFT JOIN AccountBalance ab
                    ON ab.account.id = a.id
                    AND ab.branch.id = :branchId
                    AND ab.tenantId = :tenantId
                WHERE a.tenantId = :tenantId
                    AND a.branchId = :branchId
                    AND (
                        :search IS NULL
                        OR LOWER(a.code) LIKE LOWER(CONCAT('%', :search, '%'))
                        OR LOWER(a.name) LIKE LOWER(CONCAT('%', :search, '%'))
                    )
                    AND (
                        :type IS NULL
                        OR a.type = :type
                    )
                    AND (
                        :active IS NULL
                        OR a.active = :active
                    )
                    AND (
                        :balanceFilter IS NULL
                        OR (
                            :balanceFilter = 'POSITIVE'
                            AND COALESCE(ab.balance, 0) > 0
                        )
                        OR (
                            :balanceFilter = 'NEGATIVE'
                            AND COALESCE(ab.balance, 0) < 0
                        )
                        OR (
                            :balanceFilter = 'ZERO'
                            AND COALESCE(ab.balance, 0) = 0
                        )
                        OR (
                            :balanceFilter = 'NON_ZERO'
                            AND COALESCE(ab.balance, 0) <> 0
                        )
                    )
            """)
    Page<AccountListItemProjection> findAccountListItems(
            UUID tenantId,
            UUID branchId,
            String search,
            AccountType type,
            Boolean active,
            String balanceFilter,
            Pageable pageable
    );

    Optional<Account> findByTenantIdAndBranchIdAndId(UUID tenantId, UUID branchId, UUID accountId);

    Optional<Account> findByTenantIdAndBranchIdAndCode(
            UUID tenantId,
            UUID branchId,
            String code
    );

    Page<Account> findByTenantIdAndBranchIdAndActiveTrue(
            UUID tenantId,
            UUID branchId,
            Pageable pageable
    );

    Optional<Account> findByTenantIdAndBranchIdAndRole(
            UUID tenantId,
            UUID branchId,
            AccountRole role
    );

    Optional<Account> findByTenantIdAndType(UUID tenantId, AccountType accountType);

    List<Account> findByTenantIdAndBranchId(UUID tenantId, UUID branchId);

    List<Account> findByTenantId(UUID tenantId);
}