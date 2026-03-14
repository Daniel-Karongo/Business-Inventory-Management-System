package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.GovernanceAuditService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.domain.*;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.domain.enums.*;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.dto.*;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.policy.BudgetPolicy;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.repository.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import com.IntegrityTechnologies.business_manager.security.BranchTenantGuard;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class BudgetService {

    private final BudgetRepository budgetRepository;
    private final BranchRepository branchRepository;
    private final BudgetMonthlySnapshotRepository snapshotRepository;
    private final AccountRepository accountRepository;
    private final GovernanceAuditService auditService;
    private final BudgetPolicy budgetPolicy;
    private final BranchTenantGuard branchTenantGuard;

    /* ============================================================
       CREATE BUDGET
    ============================================================ */

    @Transactional
    public Budget createBudget(
            UUID branchId,
            int fiscalYear,
            String username
    ) {

        branchTenantGuard.validate(branchId);

        Branch branch = branchRepository.findById(branchId)
                .orElseThrow(() -> new IllegalArgumentException("Branch not found"));

        int nextVersion = resolveNextVersion(
                        TenantContext.getTenantId(),
                        branchId,
                        fiscalYear,
                        BudgetScenario.BASELINE
                );

        Budget budget = Budget.builder()
                .branch(branch)
                .fiscalYear(fiscalYear)
                .versionNumber(nextVersion)
                .scenario(BudgetScenario.BASELINE)
                .status(BudgetStatus.DRAFT)
                .createdBy(username)
                .build();

        budget.setBranchId(branchId);
        budget.setTenantId(branch.getTenantId());

        return budgetRepository.save(budget);
    }

    private int resolveNextVersion(
            UUID tenantId,
            UUID branchId,
            int fiscalYear,
            BudgetScenario scenario
    ) {

        return budgetRepository
                .findTopByTenantIdAndBranchIdAndFiscalYearAndScenarioOrderByVersionNumberDesc(
                        tenantId,
                        branchId,
                        fiscalYear,
                        scenario
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

        Budget budget = budgetRepository.findByTenantIdAndId(
                TenantContext.getTenantId(),budgetId
        )
        .orElseThrow(() -> new IllegalArgumentException("Budget not found"));

        budget.ensureEditable();

        var account = accountRepository.findById(accountId)
                .orElseThrow(() -> new IllegalArgumentException("Account not found"));

        budgetPolicy.validate(account);

        BudgetLine line = budget.getLines()
                .stream()
                .filter(l -> l.getAccount().getId().equals(accountId))
                .findFirst()
                .orElseGet(() -> {

                    BudgetLine newLine = BudgetLine.builder()
                            .budget(budget)
                            .account(account)
                            .build();

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
       VARIANCE
    ============================================================ */

    public CorporateVarianceDTO computeCorporateVariance(
            int fiscalYear,
            int monthNumber,
            UUID accountId
    ) {

        Object[] row =
                snapshotRepository.aggregateCorporateVariance(
                        TenantContext.getTenantId(),
                        fiscalYear,
                        monthNumber,
                        accountId
                );

        BigDecimal planned =
                row[0] != null ? (BigDecimal) row[0] : BigDecimal.ZERO;

        BigDecimal actual =
                row[1] != null ? (BigDecimal) row[1] : BigDecimal.ZERO;

        BigDecimal variance =
                row[2] != null ? (BigDecimal) row[2] : BigDecimal.ZERO;

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

        branchTenantGuard.validate(branchId);

        Optional<BudgetMonthlySnapshot> snapshot =
                snapshotRepository.findByTenantIdAndBranchIdAndFiscalYearAndMonthNumberAndAccountId(
                        TenantContext.getTenantId(),
                        branchId,
                        fiscalYear,
                        monthNumber,
                        accountId
                );

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
                        TenantContext.getTenantId(),
                        fiscalYear,
                        monthNumber,
                        accountId
                );

        Set<UUID> ids =
                rows.stream().map(r -> (UUID) r[0]).collect(Collectors.toSet());

        Map<UUID, String> branchMap =
                branchRepository.findAllById(ids)
                        .stream()
                        .collect(Collectors.toMap(
                                Branch::getId,
                                Branch::getName
                        ));

        List<BranchComparisonDTO> result = new ArrayList<>();

        for (Object[] row : rows) {

            UUID branchId = (UUID) row[0];

            BigDecimal planned =
                    row[1] != null ? (BigDecimal) row[1] : BigDecimal.ZERO;

            BigDecimal actual =
                    row[2] != null ? (BigDecimal) row[2] : BigDecimal.ZERO;

            BigDecimal variance =
                    row[3] != null ? (BigDecimal) row[3] : BigDecimal.ZERO;

            BigDecimal variancePercent =
                    planned.compareTo(BigDecimal.ZERO) == 0
                            ? BigDecimal.ZERO
                            : variance.divide(planned, 4, java.math.RoundingMode.HALF_UP)
                            .multiply(BigDecimal.valueOf(100));

            result.add(
                    BranchComparisonDTO.builder()
                            .branchId(branchId)
                            .branchName(branchMap.getOrDefault(branchId, "Unknown"))
                            .planned(planned)
                            .actual(actual)
                            .variance(variance)
                            .variancePercent(variancePercent)
                            .build()
            );
        }

        result.sort((a, b) -> b.getVariance().compareTo(a.getVariance()));

        return result;
    }

    /* ============================================================
       WORKFLOW
    ============================================================ */

    @Transactional
    public void submitBudget(UUID budgetId, String user) {

        Budget budget =
                budgetRepository.findByTenantIdAndId(
                TenantContext.getTenantId(),budgetId
        ).orElseThrow(() -> new IllegalArgumentException("Budget not found"));

        if (budget.getStatus() != BudgetStatus.DRAFT) {
            throw new IllegalStateException("Only DRAFT budgets can be submitted");
        }

        budget.setStatus(BudgetStatus.SUBMITTED);
        budget.setSubmittedBy(user);
        budget.setSubmittedAt(LocalDateTime.now());

        auditService.log(
                budget.getBranchId(),
                "BUDGET_SUBMITTED",
                user,
                "Budget ID: " + budgetId
        );
    }

    @Transactional
    public void approveBudget(UUID budgetId, String adminUser) {

        Budget budget =
                budgetRepository.findByTenantIdAndId(
                TenantContext.getTenantId(),budgetId
        ).orElseThrow(() -> new IllegalArgumentException("Budget not found"));

        if (budget.getStatus() != BudgetStatus.SUBMITTED) {
            throw new IllegalStateException("Only SUBMITTED budgets can be approved");
        }

        budget.setStatus(BudgetStatus.APPROVED);
        budget.setApprovedBy(adminUser);
        budget.setApprovedAt(LocalDateTime.now());

        auditService.log(
                budget.getBranchId(),
                "BUDGET_APPROVED",
                adminUser,
                "Budget ID: " + budgetId
        );

        initializeBudgetSnapshots(budget);
    }

    /* ============================================================
       SNAPSHOT INITIALIZATION
    ============================================================ */

    private void initializeBudgetSnapshots(Budget budget) {

        UUID branchId = budget.getBranchId();
        UUID tenantId = budget.getTenantId();
        int year = budget.getFiscalYear();

        for (BudgetLine line : budget.getLines()) {

            UUID accountId = line.getAccount().getId();

            line.getMonths().forEach(month -> {

                snapshotRepository.findByTenantIdAndBranchIdAndFiscalYearAndMonthNumberAndAccountId(
                        tenantId,
                        branchId,
                        year,
                        month.getMonthNumber(),
                        accountId
                ).orElseGet(() ->
                        snapshotRepository.save(
                                BudgetMonthlySnapshot.builder()
                                        .tenantId(tenantId)
                                        .branchId(branchId)
                                        .fiscalYear(year)
                                        .monthNumber(month.getMonthNumber())
                                        .accountId(accountId)
                                        .planned(month.getPlannedAmount())
                                        .actual(BigDecimal.ZERO)
                                        .variance(month.getPlannedAmount().negate())
                                        .updatedAt(LocalDateTime.now())
                                        .build()
                        )
                );

            });
        }
    }
}