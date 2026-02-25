package com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountBalance;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.mapper.UserMapper;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface AccountBalanceRepository
        extends JpaRepository<AccountBalance, UUID> {

    Optional<AccountBalance> findByAccount_IdAndBranch_Id(UUID accountId, UUID branchId);

    Optional<AccountBalance> findByAccountId(UUID accountId);
}