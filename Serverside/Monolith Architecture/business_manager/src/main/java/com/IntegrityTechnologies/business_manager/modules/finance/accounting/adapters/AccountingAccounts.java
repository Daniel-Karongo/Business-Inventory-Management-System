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

    private final Map<String, Map<AccountRole, UUID>> cache = new ConcurrentHashMap<>();

    public UUID get(UUID tenantId, UUID branchId, AccountRole role) {

        String key = tenantId + ":" + branchId;

        Map<AccountRole, UUID> branchMap =
                cache.computeIfAbsent(key, k -> new ConcurrentHashMap<>());

        return branchMap.computeIfAbsent(role, r ->
                repo.findByTenantIdAndBranchIdAndRole(tenantId, branchId, r)
                        .orElseThrow(() ->
                                new IllegalStateException("Missing account role: " + r))
                        .getId()
        );
    }

    public void clearCache(UUID tenantId, UUID branchId) {
        cache.remove(tenantId + ":" + branchId);
    }
}