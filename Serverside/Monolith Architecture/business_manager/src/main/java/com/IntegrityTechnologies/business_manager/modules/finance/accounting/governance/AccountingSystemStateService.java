package com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.config.AccountingProperties;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountingMode;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import com.IntegrityTechnologies.business_manager.security.BranchTenantGuard;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class AccountingSystemStateService {

    private final AccountingSystemStateRepository repository;
    private final AccountingProperties properties;
    private final GovernanceAuditService auditService;
    private final BranchTenantGuard branchTenantGuard;

    @Transactional
    public AccountingSystemState getOrCreate(UUID branchId) {
        UUID tenantId = TenantContext.getTenantId();

        return repository.findByTenantIdAndBranchId(tenantId, branchId)
                .orElseGet(() -> {
                    AccountingSystemState s = new AccountingSystemState();
                    s.setTenantId(tenantId);
                    s.setBranchId(branchId);
                    s.setAccountingMode(properties.getMode()); // default from app config
                    return repository.save(s);
                });
    }

    @Transactional(readOnly = true)
    public AccountingSystemState getState(UUID branchId) {
        branchTenantGuard.validate(branchId);
        UUID tenantId = TenantContext.getTenantId();

        return repository.findByTenantIdAndBranchId(tenantId, branchId)
                .orElseThrow(() -> new IllegalStateException("System state missing for branch and tenant"));
    }

    @Transactional(readOnly = true)
    public AccountingMode getMode(UUID branchId) {
        branchTenantGuard.validate(branchId);
        return getOrCreate(branchId).getAccountingMode();
    }

    @Transactional
    public void updateMode(UUID branchId, AccountingMode mode, String user) {
        branchTenantGuard.validate(branchId);

        AccountingSystemState state = getOrCreate(branchId);

        if (state.isLocked()) {
            throw new IllegalStateException("Accounting mode is locked for this branch.");
        }

        AccountingMode previous = state.getAccountingMode();

        state.setAccountingMode(mode);
        repository.save(state);

        auditService.log(
                branchId,
                "ACCOUNTING_MODE_CHANGED",
                user,
                "Mode changed from " + previous + " to " + mode
        );
    }

    @Transactional
    public void lockIfNecessary(UUID branchId) {
        branchTenantGuard.validate(branchId);

        AccountingSystemState state = getOrCreate(branchId);

        if (!state.isLocked()) {

            state.setLocked(true);
            state.setLockedAt(LocalDateTime.now());
            repository.save(state);

            auditService.log(
                    branchId,
                    "ACCOUNTING_MODE_LOCKED",
                    "SYSTEM",
                    "Accounting mode locked after first journal posting"
            );
        }
    }
}