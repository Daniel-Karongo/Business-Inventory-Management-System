package com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountRole;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

@Component
@RequiredArgsConstructor
public class AccountingAccounts {

    private final AccountRepository repo;

    private final Map<UUID, Map<AccountRole, UUID>> cache = new ConcurrentHashMap<>();

    public UUID get(UUID branchId, AccountRole role) {

        Map<AccountRole, UUID> branchMap =
                cache.computeIfAbsent(branchId, id -> new ConcurrentHashMap<>());

        return branchMap.computeIfAbsent(role, r ->
                repo.findByBranchIdAndRole(branchId, r)
                        .orElseThrow(() ->
                                new IllegalStateException("Missing account role: " + r))
                        .getId()
        );
    }

    public void clearCache(UUID branchId) {
        cache.remove(branchId);
    }
}