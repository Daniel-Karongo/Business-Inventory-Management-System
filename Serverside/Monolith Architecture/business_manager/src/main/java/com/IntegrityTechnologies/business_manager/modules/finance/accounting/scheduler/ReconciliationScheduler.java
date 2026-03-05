package com.IntegrityTechnologies.business_manager.modules.finance.accounting.scheduler;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.config.ReconciliationProperties;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountBalance;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.ReconciliationState;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.ReconciliationStateRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.service.BalanceReconciliationService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.UUID;

@Slf4j
@Component
@RequiredArgsConstructor
public class ReconciliationScheduler {

    private final BalanceReconciliationService service;
    private final ReconciliationProperties properties;
    private final ReconciliationStateRepository stateRepository;
    private final BranchRepository branchRepository;

    @Scheduled(cron = "${accounting.reconciliation.cron}")
    public void runNightlyReconciliation() {

        if (!properties.isEnabled()) {
            return;
        }

        log.info("Starting nightly branch reconciliation...");

        int branchPage = 0;
        Page<Branch> branchBatch;

        do {
            branchBatch = branchRepository.findAll(PageRequest.of(branchPage++, 200));

            for (Branch branch : branchBatch.getContent()) {

                UUID branchId = branch.getId();

                log.info("Reconciling branch {}", branchId);

                long inconsistencies = 0;

                int balancePage = 0;
                Page<AccountBalance> balanceBatch;

                do {
                    balanceBatch =
                            service.getBalancesPage(
                                    branchId,
                                    balancePage++,
                                    500
                            );

                    for (AccountBalance bal : balanceBatch.getContent()) {

                        var result =
                                service.reconcileAccount(
                                        bal.getAccount().getId(),
                                        branchId,
                                        properties.isAutoRepair(),
                                        "SYSTEM"
                                );

                        if (!result.isConsistent()) {
                            inconsistencies++;
                        }
                    }

                } while (balanceBatch.hasNext());

                if (inconsistencies > 0) {
                    log.error("Branch {} has {} inconsistencies",
                            branchId,
                            inconsistencies);
                }

                ReconciliationState state =
                        stateRepository.findByBranchId(branchId)
                                .orElseGet(() -> {
                                    ReconciliationState s = new ReconciliationState();
                                    s.setBranchId(branchId);
                                    return s;
                                });

                state.setLastRunAt(LocalDateTime.now());
                state.setInconsistenciesDetected(inconsistencies);
                state.setAutoRepairEnabled(properties.isAutoRepair());

                stateRepository.save(state);
            }

        } while (branchBatch.hasNext());

        log.info("Nightly reconciliation complete.");
    }
}