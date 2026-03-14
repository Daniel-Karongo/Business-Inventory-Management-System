package com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.*;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountRole;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.data.domain.*;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface AccountRepository extends JpaRepository<Account, UUID> {

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