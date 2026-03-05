package com.IntegrityTechnologies.business_manager.modules.finance.accounting.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountBalance;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.ReconciliationResult;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.*;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountBalanceRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.LedgerEntryRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class BalanceReconciliationService {

    private final AccountBalanceRepository balanceRepo;
    private final LedgerEntryRepository ledgerRepo;
    private final ReconciliationStateRepository stateRepository;
    private final ReconciliationRunRepository runRepo;
    private final ReconciliationItemRepository itemRepo;
    private final GovernanceAuditService auditService;

    @Transactional
    public UUID runAndPersist(UUID branchId, boolean repair, String user) {

        ReconciliationRun run =
                runRepo.save(new ReconciliationRun(branchId, repair));

        // 1️⃣ Single aggregation query
        List<Object[]> ledgerData =
                ledgerRepo.computeBranchBalances(
                        branchId,
                        EntryDirection.DEBIT
                );

        Map<UUID, BigDecimal> ledgerMap =
                ledgerData.stream()
                        .collect(Collectors.toMap(
                                r -> (UUID) r[0],
                                r -> (BigDecimal) r[1]
                        ));

        long inconsistencies = 0;
        int batchSize = 1000;
        List<ReconciliationItem> batch = new ArrayList<>(batchSize);

        // 2️⃣ Stream projections in pages
        int pageIndex = 0;
        Page<AccountBalance> page;

        do {
            page = balanceRepo.findByBranch_Id(
                    branchId,
                    PageRequest.of(pageIndex++, 2000)
            );

            for (AccountBalance projection : page.getContent()) {

                UUID accountId = projection.getAccount().getId();

                BigDecimal ledgerBalance =
                        ledgerMap.getOrDefault(accountId, BigDecimal.ZERO);

                BigDecimal projected = projection.getBalance();

                boolean matches =
                        ledgerBalance.compareTo(projected) == 0;

                if (!matches) inconsistencies++;

                if (!matches && repair) {
                    projection.setBalance(ledgerBalance);
                    projection.setUpdatedAt(LocalDateTime.now());
                    balanceRepo.save(projection);
                }

                batch.add(new ReconciliationItem(
                        accountId,
                        ledgerBalance,
                        projected,
                        matches,
                        run
                ));

                // 🔥 Remove processed account from ledgerMap
                ledgerMap.remove(accountId);

                if (batch.size() >= batchSize) {
                    itemRepo.saveAll(batch);
                    itemRepo.flush();
                    batch.clear();
                }
            }

        } while (page.hasNext());

        // 3️⃣ Remaining accounts exist in ledger but not in projections
        for (var entry : ledgerMap.entrySet()) {

            UUID accountId = entry.getKey();
            BigDecimal ledgerBalance = entry.getValue();

            inconsistencies++;

            if (repair) {
                AccountBalance projection = new AccountBalance();
                projection.setBalance(ledgerBalance);
                projection.setUpdatedAt(LocalDateTime.now());
                balanceRepo.save(projection);
            }

            batch.add(new ReconciliationItem(
                    accountId,
                    ledgerBalance,
                    BigDecimal.ZERO,
                    false,
                    run
            ));

            if (batch.size() >= batchSize) {
                itemRepo.saveAll(batch);
                itemRepo.flush();
                batch.clear();
            }
        }

        if (!batch.isEmpty()) {
            itemRepo.saveAll(batch);
            itemRepo.flush();
        }

        run.complete(
                ledgerData.size(),
                inconsistencies
        );

        auditService.log(
                branchId,
                "RECONCILIATION_RUN",
                user,
                "RunId=" + run.getId() +
                        ", repair=" + repair +
                        ", inconsistencies=" + inconsistencies
        );
        
        runRepo.save(run);

        ReconciliationState state =
                stateRepository.findByBranchId(branchId)
                        .orElseGet(() -> {
                            ReconciliationState s = new ReconciliationState();
                            s.setBranchId(branchId);
                            return s;
                        });

        state.setLastRunAt(LocalDateTime.now());
        state.setInconsistenciesDetected(inconsistencies);

        stateRepository.save(state);

        return run.getId();
    }

    @Transactional
    public ReconciliationResult reconcileAccount(
            UUID accountId,
            UUID branchId,
            boolean repair,
            String user
    ) {

        BigDecimal ledgerBalance =
                ledgerRepo.computeNetBalance(
                        accountId,
                        branchId,
                        EntryDirection.DEBIT
                );

        AccountBalance projection =
                balanceRepo.findByAccount_IdAndBranch_Id(accountId, branchId)
                        .orElse(null);

        BigDecimal projected =
                projection != null ? projection.getBalance() : BigDecimal.ZERO;

        boolean matches = ledgerBalance.compareTo(projected) == 0;

        if (!matches && repair) {

            if (projection == null) {
                projection = new AccountBalance();
                projection.setBalance(BigDecimal.ZERO);
            }

            projection.setBalance(ledgerBalance);
            projection.setUpdatedAt(LocalDateTime.now());

            balanceRepo.save(projection);
        }
        auditService.log(
                branchId,
                "ACCOUNT_RECONCILED",
                user,
                "AccountId=" + accountId +
                        ", repair=" + repair +
                        ", matched=" + matches
        );

        return new ReconciliationResult(
                accountId,
                branchId,
                ledgerBalance,
                projected,
                matches
        );
    }

    @Transactional
    public void setAutoRepair(UUID branchId, boolean enabled, String user) {

        ReconciliationState state =
                stateRepository.findByBranchId(branchId)
                        .orElseGet(() -> {
                            ReconciliationState s = new ReconciliationState();
                            s.setBranchId(branchId);
                            return s;
                        });

        state.setAutoRepairEnabled(enabled);
        stateRepository.save(state);

        auditService.log(
                branchId,
                "AUTO_REPAIR_TOGGLED",
                user,
                "Auto repair set to: " + enabled
        );
    }

    @Transactional(readOnly = true)
    public Page<AccountBalance> getBalancesPage(
            UUID branchId,
            int page,
            int size
    ) {
        return balanceRepo.findByBranch_Id(
                branchId,
                PageRequest.of(page, size)
        );
    }
}