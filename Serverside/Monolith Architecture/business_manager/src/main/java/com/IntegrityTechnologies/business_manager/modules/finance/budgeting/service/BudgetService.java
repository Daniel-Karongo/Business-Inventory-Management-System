package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.LedgerEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.domain.*;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.domain.enums.BudgetScenario;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.dto.BranchComparisonDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.dto.CorporateVarianceDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.repository.BudgetMonthlySnapshotRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.repository.BudgetRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;

import static com.IntegrityTechnologies.business_manager.modules.finance.accounting.support.AccountingSignRules.*;

@Service
@RequiredArgsConstructor
public class BudgetService {

    private final BudgetRepository budgetRepository;
    private final BranchRepository branchRepository;
    private final LedgerEntryRepository ledgerRepository;
    private final BudgetMonthlySnapshotRepository snapshotRepository;

    /* ============================================================
       CREATE NEW BUDGET (AUTO VERSIONING)
       ============================================================ */

    @Transactional
    public Budget createBudget(
            UUID branchId,
            int fiscalYear,
            String username
    ) {

        BudgetScenario scenario = BudgetScenario.BASELINE;

        int nextVersion = resolveNextVersion(branchId, fiscalYear, scenario);

        Branch branch = null;

        if (branchId != null) {
            branch = branchRepository.findById(branchId)
                    .orElseThrow(() -> new IllegalArgumentException("Branch not found"));
        }

        Budget budget = Budget.builder()
                .branch(branch)
                .fiscalYear(fiscalYear)
                .versionNumber(nextVersion)
                .scenario(scenario)
                .locked(false)
                .createdBy(username)
                .build();

        return budgetRepository.save(budget);
    }

    private int resolveNextVersion(
            UUID branchId,
            int fiscalYear,
            BudgetScenario scenario
    ) {

        if (branchId == null) {
            return budgetRepository
                    .findTopByBranchIsNullAndFiscalYearAndScenarioOrderByVersionNumberDesc(
                            fiscalYear, scenario
                    )
                    .map(b -> b.getVersionNumber() + 1)
                    .orElse(1);
        }

        return budgetRepository
                .findTopByBranch_IdAndFiscalYearAndScenarioOrderByVersionNumberDesc(
                        branchId, fiscalYear, scenario
                )
                .map(b -> b.getVersionNumber() + 1)
                .orElse(1);
    }

    /* ============================================================
       UPSERT BUDGET LINE + MONTH
       ============================================================ */

    @Transactional
    public void upsertBudgetMonth(
            UUID budgetId,
            UUID accountId,
            int monthNumber,
            BigDecimal amount
    ) {

        Budget budget = budgetRepository.findById(budgetId)
                .orElseThrow(() -> new IllegalArgumentException("Budget not found"));

        budget.ensureEditable();

        BudgetLine line = budget.getLines()
                .stream()
                .filter(l -> l.getAccount().getId().equals(accountId))
                .findFirst()
                .orElseGet(() -> {
                    BudgetLine newLine = BudgetLine.builder()
                            .budget(budget)
                            .account(new com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.Account())
                            .build();

                    newLine.getAccount().setId(accountId);
                    budget.getLines().add(newLine);
                    return newLine;
                });

        Optional<BudgetMonth> existing =
                line.getMonths()
                        .stream()
                        .filter(m -> m.getMonthNumber() == monthNumber)
                        .findFirst();

        if (existing.isPresent()) {
            existing.get().setPlannedAmount(amount);
        } else {
            line.getMonths().add(
                    BudgetMonth.builder()
                            .budgetLine(line)
                            .monthNumber(monthNumber)
                            .plannedAmount(amount)
                            .build()
            );
        }
    }

    /* ============================================================
       RESOLVE BUDGET (BRANCH OVERRIDE → GLOBAL FALLBACK)
       ============================================================ */

    public Optional<Budget> resolveActiveBudget(
            UUID branchId,
            int fiscalYear
    ) {

        BudgetScenario scenario = BudgetScenario.BASELINE;

        if (branchId != null) {
            Optional<Budget> branchBudget =
                    budgetRepository
                            .findTopByBranch_IdAndFiscalYearAndScenarioOrderByVersionNumberDesc(
                                    branchId,
                                    fiscalYear,
                                    scenario
                            );

            if (branchBudget.isPresent()) {
                return branchBudget;
            }
        }

        return budgetRepository
                .findTopByBranchIsNullAndFiscalYearAndScenarioOrderByVersionNumberDesc(
                        fiscalYear,
                        scenario
                );
    }

    /* ============================================================
       SNAPSHOT GENERATION
       ============================================================ */

    @Transactional
    public void computeMonthlySnapshot(
            UUID branchId,
            int fiscalYear,
            int monthNumber
    ) {

        Optional<Budget> budgetOpt =
                resolveActiveBudget(branchId, fiscalYear);

        if (budgetOpt.isEmpty()) return;

        Budget budget = budgetOpt.get();

        LocalDate start = LocalDate.of(fiscalYear, monthNumber, 1);
        LocalDate end = start.plusMonths(1).minusDays(1);

        for (BudgetLine line : budget.getLines()) {

            UUID accountId = line.getAccount().getId();

            BigDecimal planned =
                    line.getMonths()
                            .stream()
                            .filter(m -> m.getMonthNumber() == monthNumber)
                            .map(BudgetMonth::getPlannedAmount)
                            .findFirst()
                            .orElse(BigDecimal.ZERO);

            BigDecimal actual =
                    ledgerRepository.netMovementForAccount(
                            accountId,
                            start.atStartOfDay(),
                            end.atTime(23,59,59),
                            DEBIT_NORMAL,
                            CREDIT_NORMAL,
                            DEBIT,
                            CREDIT
                    );

            BigDecimal variance =
                    calculateVariance(line.getAccount().getType(), planned, actual);

            snapshotRepository.save(
                    BudgetMonthlySnapshot.builder()
                            .branchId(branchId)
                            .fiscalYear(fiscalYear)
                            .monthNumber(monthNumber)
                            .accountId(accountId)
                            .planned(planned)
                            .actual(actual)
                            .variance(variance)
                            .computedAt(LocalDateTime.now())
                            .build()
            );
        }
    }

    private BigDecimal calculateVariance(
            AccountType type,
            BigDecimal planned,
            BigDecimal actual
    ) {

        if (type == AccountType.INCOME) {
            return actual.subtract(planned);
        }

        if (type == AccountType.EXPENSE) {
            return planned.subtract(actual);
        }

        return actual.subtract(planned);
    }

    public CorporateVarianceDTO computeCorporateVariance(
            int fiscalYear,
            int monthNumber,
            UUID accountId
    ) {

        Object[] row = snapshotRepository
                .aggregateCorporateVariance(fiscalYear, monthNumber, accountId);

        BigDecimal planned = row[0] != null ? (BigDecimal) row[0] : BigDecimal.ZERO;
        BigDecimal actual = row[1] != null ? (BigDecimal) row[1] : BigDecimal.ZERO;
        BigDecimal variance = row[2] != null ? (BigDecimal) row[2] : BigDecimal.ZERO;

        return CorporateVarianceDTO.builder()
                .fiscalYear(fiscalYear)
                .month(monthNumber)
                .accountId(accountId)
                .planned(planned)
                .actual(actual)
                .variance(variance)
                .build();
    }

    public CorporateVarianceDTO computeBranchVariance(
            UUID branchId,
            int fiscalYear,
            int monthNumber,
            UUID accountId
    ) {

        Optional<BudgetMonthlySnapshot> snapshot =
                snapshotRepository.findByBranchIdAndFiscalYearAndMonthNumberAndAccountId(
                        branchId,
                        fiscalYear,
                        monthNumber,
                        accountId
                );

        // 🔥 If missing, compute automatically
        if (snapshot.isEmpty()) {
            computeMonthlySnapshot(branchId, fiscalYear, monthNumber);

            snapshot =
                    snapshotRepository.findByBranchIdAndFiscalYearAndMonthNumberAndAccountId(
                            branchId,
                            fiscalYear,
                            monthNumber,
                            accountId
                    );
        }

        return snapshot
                .map(s -> CorporateVarianceDTO.builder()
                        .fiscalYear(fiscalYear)
                        .month(monthNumber)
                        .accountId(accountId)
                        .planned(s.getPlanned())
                        .actual(s.getActual())
                        .variance(s.getVariance())
                        .build())
                .orElse(
                        CorporateVarianceDTO.builder()
                                .fiscalYear(fiscalYear)
                                .month(monthNumber)
                                .accountId(accountId)
                                .planned(BigDecimal.ZERO)
                                .actual(BigDecimal.ZERO)
                                .variance(BigDecimal.ZERO)
                                .build()
                );
    }

    public List<BranchComparisonDTO> compareBranches(
            int fiscalYear,
            int monthNumber,
            UUID accountId
    ) {

        List<Object[]> rows =
                snapshotRepository.aggregateByBranch(
                        fiscalYear,
                        monthNumber,
                        accountId
                );

        List<BranchComparisonDTO> result = new ArrayList<>();

        for (Object[] row : rows) {

            UUID branchId = (UUID) row[0];
            BigDecimal planned = row[1] != null ? (BigDecimal) row[1] : BigDecimal.ZERO;
            BigDecimal actual = row[2] != null ? (BigDecimal) row[2] : BigDecimal.ZERO;
            BigDecimal variance = row[3] != null ? (BigDecimal) row[3] : BigDecimal.ZERO;

            BigDecimal variancePercent =
                    planned.compareTo(BigDecimal.ZERO) == 0
                            ? BigDecimal.ZERO
                            : variance
                            .divide(planned, 4, java.math.RoundingMode.HALF_UP)
                            .multiply(BigDecimal.valueOf(100));

            String branchName =
                    branchRepository.findById(branchId)
                            .map(b -> b.getName())
                            .orElse("Unknown");

            result.add(
                    BranchComparisonDTO.builder()
                            .branchId(branchId)
                            .branchName(branchName)
                            .planned(planned)
                            .actual(actual)
                            .variance(variance)
                            .variancePercent(variancePercent)
                            .build()
            );
        }

        // Sort best performing first (highest positive variance)
        result.sort((a, b) ->
                b.getVariance().compareTo(a.getVariance())
        );

        return result;
    }
}