package com.IntegrityTechnologies.business_manager.modules.finance.accounting.support;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.Account;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Component;

import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

@Component
@RequiredArgsConstructor
public class AccountRoleResolver {

    private final AccountRepository accountRepository;

    @Cacheable(value = "accountRoleLookup", key = "#tenantId")
    public Set<Account> getTenantAccounts(UUID tenantId) {
        return accountRepository.findByTenantId(tenantId)
                .stream()
                .collect(Collectors.toSet());
    }

    public Set<String> resolveCashAccounts(UUID tenantId, UUID branchId) {

        return getTenantAccounts(tenantId)
                .stream()
                .filter(a ->
                        branchId.equals(a.getBranchId()) &&
                                (
                                        a.getRole() == "CASH" ||
                                                a.getRole() == "BANK" ||
                                                a.getRole() == "MPESA"
                                )
                )
                .map(Account::getCode)
                .collect(Collectors.toSet());
    }

    public String resolveReceivableAccount(UUID tenantId, UUID branchId) {

        return getTenantAccounts(tenantId)
                .stream()
                .filter(a ->
                        a.getRole() == "ACCOUNTS_RECEIVABLE" &&
                                branchId.equals(a.getBranchId())
                )
                .findFirst()
                .orElseThrow()
                .getCode();
    }

    public String resolvePayableAccount(UUID tenantId, UUID branchId) {

        return getTenantAccounts(tenantId)
                .stream()
                .filter(a ->
                        a.getRole() == "ACCOUNTS_PAYABLE" &&
                                branchId.equals(a.getBranchId())
                )
                .findFirst()
                .orElseThrow()
                .getCode();
    }

    public Set<String> resolveReceivableAccountsMultiBranch(UUID tenantId) {

        return getTenantAccounts(tenantId)
                .stream()
                .filter(a -> a.getRole() == "ACCOUNTS_RECEIVABLE")
                .map(Account::getCode)
                .collect(Collectors.toSet());
    }

    public Set<String> resolvePayableAccountsMultiBranch(UUID tenantId) {

        return getTenantAccounts(tenantId)
                .stream()
                .filter(a -> a.getRole() == "ACCOUNTS_PAYABLE")
                .map(Account::getCode)
                .collect(Collectors.toSet());
    }

    public Set<String> resolveCashAccountsMultiBranch(UUID tenantId) {

        return getTenantAccounts(tenantId)
                .stream()
                .filter(a ->
                        a.getType() == AccountType.ASSET &&
                                (
                                        a.getRole() == "CASH" ||
                                                a.getRole() == "BANK" ||
                                                a.getRole() == "MPESA"
                                )
                )
                .map(Account::getCode)
                .collect(Collectors.toSet());
    }
}