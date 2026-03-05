package com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.config.AccountingProperties;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountingMode;
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

    @Transactional
    public AccountingSystemState getOrCreate(UUID branchId) {

        return repository.findByBranchId(branchId)
                .orElseGet(() -> {
                    AccountingSystemState s = new AccountingSystemState();
                    s.setBranchId(branchId);
                    s.setAccountingMode(properties.getMode()); // default from app config
                    return repository.save(s);
                });
    }

    @Transactional(readOnly = true)
    public AccountingSystemState getState(UUID branchId) {
        return repository.findByBranchId(branchId)
                .orElseThrow(() -> new IllegalStateException("System state missing for branch"));
    }

    @Transactional(readOnly = true)
    public AccountingMode getMode(UUID branchId) {
        return getOrCreate(branchId).getAccountingMode();
    }

    @Transactional
    public void updateMode(UUID branchId, AccountingMode mode, String user) {

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