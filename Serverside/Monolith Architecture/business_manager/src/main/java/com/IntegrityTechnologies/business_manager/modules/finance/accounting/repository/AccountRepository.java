package com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.Account;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountRole;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface AccountRepository extends JpaRepository<Account, UUID> {
    Optional<Account> findByBranchIdAndCode(UUID branchId, String code);

    Page<Account> findByBranchIdAndActiveTrue(UUID branchId, Pageable pageable);
    Optional<Account> findByBranchIdAndRole(UUID branchId, AccountRole role);
}