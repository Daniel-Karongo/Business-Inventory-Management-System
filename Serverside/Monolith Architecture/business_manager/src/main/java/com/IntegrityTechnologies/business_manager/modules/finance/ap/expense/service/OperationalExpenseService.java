package com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.Account;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.service.AutoFundingService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.service.OperationalExpensePostingService;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.domain.ExpenseStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.domain.OperationalExpense;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.domain.OperationalExpenseSettlement;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.dto.*;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.repository.OperationalExpenseRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.repository.OperationalExpenseSettlementRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class OperationalExpenseService {

    private final OperationalExpenseRepository expenseRepository;

    private final OperationalExpenseSettlementRepository settlementRepository;

    private final OperationalExpensePostingService postingService;
    private final AutoFundingService autoFundingService;
    private final AccountingFacade accountingFacade;
    private final AccountRepository accountRepository;

    @Transactional(readOnly = true)
    public Page<OperationalExpenseDTO> list(
            UUID branchId,
            Pageable pageable,
            String search,
            ExpenseStatus status
    ) {

        UUID tenantId =
                TenantContext.getTenantId();

        Page<OperationalExpense> page;

        if (
                search != null
                        &&
                        !search.isBlank()
        ) {
            page =
                    expenseRepository.search(
                            tenantId,
                            branchId,
                            search,
                            pageable
                    );
        } else if (
                status != null
        ) {
            page =
                    expenseRepository
                            .findByTenantIdAndBranchIdAndStatus(
                                    tenantId,
                                    branchId,
                                    status,
                                    pageable
                            );
        } else {
            page =
                    expenseRepository
                            .findByTenantIdAndBranchId(
                                    tenantId,
                                    branchId,
                                    pageable
                            );
        }

        return page.map(
                this::toDto
        );
    }

    @Transactional(readOnly = true)
    public OperationalExpenseWorkspaceDTO workspace(
            UUID branchId,
            UUID expenseId
    ) {

        OperationalExpense expense =
                getExpense(
                        branchId,
                        expenseId
                );

        List<OperationalExpenseSettlementDTO>
                settlements =
                settlementRepository
                        .findByTenantIdAndBranchIdAndExpenseId(
                                TenantContext.getTenantId(),
                                branchId,
                                expenseId
                        )
                        .stream()
                        .map(
                                this::toSettlementDto
                        )
                        .toList();

        Account account =
                accountRepository
                        .findByTenantIdAndBranchIdAndId(
                                TenantContext.getTenantId(),
                                branchId,
                                expense.getExpenseAccountId()
                        )
                        .orElseThrow();

        return OperationalExpenseWorkspaceDTO
                .builder()
                .expenseId(
                        expense.getId()
                )
                .description(
                        expense.getDescription()
                )
                .expenseAccountId(
                        account.getId()
                )
                .expenseAccountName(
                        account.getName()
                )
                .expense(
                        toDto(expense)
                )
                .settlements(
                        settlements
                )
                .build();
    }

    @Transactional
    public OperationalExpense createExpense(
            CreateOperationalExpenseRequest request
    ) {
        validateExpenseAccount(
                request.getBranchId(),
                request.getExpenseAccountId()
        );

        if (request.isAutoPay()) {
            validateFundingAccount(
                    request.getBranchId(),
                    request.getFundingAccountId()
            );

            autoFundingService.ensureFundingAvailable(
                    request.getBranchId(),
                    request.getFundingAccountId(),
                    request.getAmount(),
                    request.getAccountingDate(),
                    request.getReference()
            );
        }
        UUID accountingJournalId;

        ExpenseStatus status;

        BigDecimal settledAmount;

        UUID paymentJournalId = null;

        UUID accrualJournalId = null;

        if (request.isAutoPay()) {

            accountingJournalId =
                    postingService.postPaidExpense(
                            request.getBranchId(),
                            request.getFundingAccountId(),
                            request.getExpenseAccountId(),
                            request.getReference(),
                            request.getDescription(),
                            request.getAmount(),
                            request.getAccountingDate(),
                            request.getSourceId()
                    );

            paymentJournalId =
                    accountingJournalId;

            status =
                    ExpenseStatus.SETTLED;

            settledAmount =
                    request.getAmount();

        } else {

            accountingJournalId =
                    postingService.postAccrualExpense(
                            request.getBranchId(),
                            request.getExpenseAccountId(),
                            request.getReference(),
                            request.getDescription(),
                            request.getAmount(),
                            request.getAccountingDate(),
                            request.getSourceId()
                    );

            accrualJournalId =
                    accountingJournalId;

            status =
                    ExpenseStatus.OPEN;

            settledAmount =
                    BigDecimal.ZERO;
        }

        OperationalExpense expense =
                OperationalExpense.builder()
                        .branchId(
                                request.getBranchId()
                        )
                        .expenseAccountId(
                                request.getExpenseAccountId()
                        )
                        .description(
                                request.getDescription()
                        )
                        .amount(
                                request.getAmount()
                        )
                        .settledAmount(
                                settledAmount
                        )
                        .status(
                                status
                        )
                        .accountingDate(
                                request.getAccountingDate()
                        )
                        .sourceModule(
                                request.getSourceModule()
                        )
                        .sourceId(
                                request.getSourceId()
                        )
                        .accrualJournalId(
                                accrualJournalId
                        )
                        .paymentJournalId(
                                paymentJournalId
                        )
                        .reversed(false)
                        .build();

        expense =
                expenseRepository.save(
                        expense
                );

        if (request.isAutoPay()) {

            settlementRepository.save(
                    OperationalExpenseSettlement
                            .builder()
                            .branchId(
                                    request.getBranchId()
                            )
                            .expenseId(
                                    expense.getId()
                            )
                            .fundingAccountId(
                                    request.getFundingAccountId()
                            )
                            .amount(
                                    request.getAmount()
                            )
                            .settlementDate(
                                    request.getAccountingDate()
                            )
                            .reference(
                                    request.getReference()
                            )
                            .settlementJournalId(
                                    paymentJournalId
                            )
                            .sourceId(
                                    request.getSourceId()
                            )
                            .reversed(false)
                            .build()
            );
        }

        return expense;
    }

    @Transactional
    public OperationalExpense settleExpense(
            UUID branchId,
            UUID expenseId,
            SettleOperationalExpenseRequest request
    ) {

        UUID tenantId =
                TenantContext.getTenantId();

        validateFundingAccount(
                branchId,
                request.getFundingAccountId()
        );

        OperationalExpense expense =
                expenseRepository
                        .findForUpdate(
                                tenantId,
                                branchId,
                                expenseId
                        )
                        .orElseThrow(
                                () ->
                                        new IllegalArgumentException(
                                                "Expense not found"
                                        )
                        );

        if (expense.getStatus() == ExpenseStatus.REVERSED) {
            throw new IllegalStateException(
                    "Expense already reversed"
            );
        }

        BigDecimal outstanding =
                expense.getOutstandingAmount();

        if (
                request.getAmount()
                        .compareTo(outstanding) > 0
        ) {
            throw new IllegalArgumentException(
                    "Settlement exceeds outstanding amount"
            );
        }

        autoFundingService.ensureFundingAvailable(
                branchId,
                request.getFundingAccountId(),
                request.getAmount(),
                request.getSettlementDate(),
                request.getReference()
        );

        if (
                request.getSourceId()
                        != null
        ) {

            Optional<OperationalExpenseSettlement>
                    existing =
                    settlementRepository
                            .findByTenantIdAndSourceId(
                                    tenantId,
                                    request.getSourceId()
                            );

            if (existing.isPresent()) {

                return expense;
            }
        }

        UUID settlementSourceId =
                request.getSourceId() != null
                        ? request.getSourceId()
                        : UUID.randomUUID();

        UUID settlementJournalId =
                postingService.postSettlement(
                        branchId,
                        request.getFundingAccountId(),
                        request.getAmount(),
                        request.getSettlementDate(),
                        settlementSourceId,
                        request.getReference()
                );

        OperationalExpenseSettlement settlement =
                OperationalExpenseSettlement.builder()
                        .sourceId(
                                settlementSourceId
                        )
                        .branchId(branchId)
                        .expenseId(expense.getId())
                        .fundingAccountId(
                                request.getFundingAccountId()
                        )
                        .amount(
                                request.getAmount()
                        )
                        .settlementDate(
                                request.getSettlementDate()
                        )
                        .reference(
                                request.getReference()
                        )
                        .settlementJournalId(
                                settlementJournalId
                        )
                        .reversed(false)
                        .build();

        settlementRepository.save(
                settlement
        );

        recomputeStatus(
                expense
        );

        return expenseRepository.save(
                expense
        );
    }

    @Transactional
    public void reverseExpense(
            UUID branchId,
            UUID expenseId,
            String reason
    ) {
        OperationalExpense expense =
                getExpense(
                        branchId,
                        expenseId
                );

        if (
                expense.getStatus()
                        == ExpenseStatus.REVERSED
        ) {
            throw new IllegalStateException(
                    "Expense already reversed"
            );
        }

        if (
                expense.getStatus()
                        == ExpenseStatus.PARTIALLY_SETTLED
        ) {
            throw new IllegalStateException(
                    "Reverse settlements first"
            );
        }

        /*
         * Auto-paid expense
         */
        if (
                expense.getPaymentJournalId()
                        != null
        ) {

            List<OperationalExpenseSettlement>
                    settlements =
                    settlementRepository
                            .findByTenantIdAndBranchIdAndExpenseId(
                                    TenantContext.getTenantId(),
                                    branchId,
                                    expenseId
                            );

            for (
                    OperationalExpenseSettlement settlement
                    : settlements
            ) {

                if (!settlement.isReversed()) {

                    accountingFacade.reverseJournal(
                            settlement.getSettlementJournalId(),
                            reason,
                            currentUser()
                    );

                    settlement.setReversed(
                            true
                    );

                    settlementRepository.save(
                            settlement
                    );
                }
            }

            expense.setReversed(
                    true
            );

            expense.setStatus(
                    ExpenseStatus.REVERSED
            );

            expenseRepository.save(
                    expense
            );

            return;
        }

        /*
         * Accrual expense
         */
        accountingFacade.reverseJournal(
                expense.getAccrualJournalId(),
                reason,
                currentUser()
        );

        expense.setReversed(
                true
        );

        expense.setStatus(
                ExpenseStatus.REVERSED
        );

        expenseRepository.save(
                expense
        );
    }

    @Transactional
    public void reverseSettlement(
            UUID branchId,
            UUID settlementId,
            String reason
    ) {

        OperationalExpenseSettlement settlement =
                settlementRepository
                        .findByTenantIdAndBranchIdAndId(
                                TenantContext.getTenantId(),
                                branchId,
                                settlementId
                        )
                        .orElseThrow(() ->
                                new IllegalArgumentException(
                                        "Settlement not found"
                                )
                        );

        if (settlement.isReversed()) {
            throw new IllegalStateException(
                    "Settlement already reversed"
            );
        }

        accountingFacade.reverseJournal(
                settlement.getSettlementJournalId(),
                reason,
                currentUser()
        );

        settlement.setReversed(true);

        settlementRepository.save(
                settlement
        );

        OperationalExpense expense =
                getExpense(
                        branchId,
                        settlement.getExpenseId()
                );

        recomputeStatus(
                expense
        );

        expenseRepository.save(
                expense
        );
    }

    @Transactional
    public void bulkSettle(
            UUID branchId,
            BulkSettleOperationalExpenseRequest request
    ) {

        for (UUID expenseId :
                request.getExpenseIds()) {

            OperationalExpense expense =
                    expenseRepository
                            .findByTenantIdAndBranchIdAndId(
                                    TenantContext.getTenantId(),
                                    branchId,
                                    expenseId
                            )
                            .orElseThrow();

            BigDecimal outstanding =
                    expense.getOutstandingAmount();

            if (
                    outstanding.compareTo(
                            BigDecimal.ZERO
                    ) <= 0
            ) {
                continue;
            }

            SettleOperationalExpenseRequest settle =
                    new SettleOperationalExpenseRequest();

            settle.setFundingAccountId(
                    request.getFundingAccountId()
            );

            settle.setSettlementDate(
                    request.getSettlementDate()
            );

            settle.setReference(
                    request.getReference()
            );

            settle.setSourceId(
                    UUID.nameUUIDFromBytes(
                            (
                                    request.getSourceId()
                                            + "::"
                                            + expenseId
                            ).getBytes()
                    )
            );

            settle.setAmount(
                    outstanding
            );

            settleExpense(
                    branchId,
                    expenseId,
                    settle
            );
        }
    }

    @Transactional(readOnly = true)
    public List<OperationalExpense> getOpenExpenses(
            UUID branchId
    ) {

        return expenseRepository
                .findByTenantIdAndBranchIdAndStatusIn(
                        TenantContext.getTenantId(),
                        branchId,
                        List.of(
                                ExpenseStatus.OPEN,
                                ExpenseStatus.PARTIALLY_SETTLED
                        )
                );
    }

    @Transactional(readOnly = true)
    public OperationalExpense getExpense(
            UUID branchId,
            UUID expenseId
    ) {

        return expenseRepository
                .findByTenantIdAndBranchIdAndId(
                        TenantContext.getTenantId(),
                        branchId,
                        expenseId
                )
                .orElseThrow(() ->
                        new IllegalArgumentException(
                                "Expense not found"
                        )
                );
    }

    private void recomputeStatus(
            OperationalExpense expense
    ) {
        BigDecimal settled =
                settlementRepository
                        .findByTenantIdAndBranchIdAndExpenseId(
                                TenantContext.getTenantId(),
                                expense.getBranchId(),
                                expense.getId()
                        )
                        .stream()
                        .filter(
                                s -> !s.isReversed()
                        )
                        .map(
                                OperationalExpenseSettlement::getAmount
                        )
                        .reduce(
                                BigDecimal.ZERO,
                                BigDecimal::add
                        );

        expense.setSettledAmount(
                settled
        );

        if (
                settled.compareTo(
                        BigDecimal.ZERO
                ) == 0
        ) {
            expense.setStatus(
                    ExpenseStatus.OPEN
            );
        } else if (
                settled.compareTo(
                        expense.getAmount()
                ) >= 0
        ) {
            expense.setStatus(
                    ExpenseStatus.SETTLED
            );
        } else {
            expense.setStatus(
                    ExpenseStatus.PARTIALLY_SETTLED
            );
        }
    }

    private Account getFundingAccount(
            UUID branchId,
            UUID accountId
    ) {
        return accountRepository
                .findByTenantIdAndBranchIdAndId(
                        TenantContext.getTenantId(),
                        branchId,
                        accountId
                )
                .orElseThrow(
                        () ->
                                new IllegalArgumentException(
                                        "Funding account not found"
                                )
                );
    }

    private void validateFundingAccount(
            UUID branchId,
            UUID accountId
    ) {
        Account account =
                getFundingAccount(
                        branchId,
                        accountId
                );

        String role =
                account.getRole();

        if (
                !"CASH".equals(role)
                        &&
                        !"BANK".equals(role)
                        &&
                        !"MPESA".equals(role)
        ) {
            throw new IllegalArgumentException(
                    "Funding account must be CASH, BANK or MPESA"
            );
        }
    }

    private void validateExpenseAccount(
            UUID branchId,
            UUID accountId
    ) {
        Account account =
                accountRepository
                        .findByTenantIdAndBranchIdAndId(
                                TenantContext.getTenantId(),
                                branchId,
                                accountId
                        )
                        .orElseThrow(
                                () ->
                                        new IllegalArgumentException(
                                                "Expense account not found"
                                        )
                        );

        if (
                account.getType()
                        != AccountType.EXPENSE
        ) {
            throw new IllegalArgumentException(
                    "Account must be an EXPENSE account"
            );
        }
    }

    private String currentUser() {
        var auth =
                SecurityContextHolder
                        .getContext()
                        .getAuthentication();

        return auth != null
                ? auth.getName()
                : "SYSTEM";
    }

    private OperationalExpenseDTO toDto(
            OperationalExpense expense
    ) {
        return OperationalExpenseDTO
                .builder()
                .id(expense.getId())
                .expenseAccountId(
                        expense.getExpenseAccountId()
                )
                .description(
                        expense.getDescription()
                )
                .amount(
                        expense.getAmount()
                )
                .settledAmount(
                        expense.getSettledAmount()
                )
                .status(
                        expense.getStatus()
                )
                .accountingDate(
                        expense.getAccountingDate()
                )
                .build();
    }

    private OperationalExpenseSettlementDTO toSettlementDto(
            OperationalExpenseSettlement settlement
    ) {

        return OperationalExpenseSettlementDTO
                .builder()
                .id(
                        settlement.getId()
                )
                .expenseId(
                        settlement.getExpenseId()
                )
                .fundingAccountId(
                        settlement.getFundingAccountId()
                )
                .amount(
                        settlement.getAmount()
                )
                .settlementDate(
                        settlement.getSettlementDate()
                )
                .reference(
                        settlement.getReference()
                )
                .reversed(
                        settlement.isReversed()
                )
                .build();
    }
}