package com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.GovernanceAuditService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountingPeriodRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.LedgerEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.base.model.BusinessTaxMode;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.base.model.TaxSystemState;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.base.service.TaxSystemStateService;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.dto.CorporateTaxAccrualPreviewDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.dto.CorporateTaxOverviewDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.model.CorporateTaxFiling;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.model.CorporateTaxFilingStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.model.CorporateTaxPayment;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.repository.CorporateTaxFilingRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.repository.CorporateTaxLedgerProjectionRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.repository.CorporateTaxPaymentRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import static com.IntegrityTechnologies.business_manager.modules.finance.accounting.support.AccountingSignRules.CREDIT;
import static com.IntegrityTechnologies.business_manager.modules.finance.accounting.support.AccountingSignRules.CREDIT_NORMAL;
import static com.IntegrityTechnologies.business_manager.modules.finance.accounting.support.AccountingSignRules.DEBIT;
import static com.IntegrityTechnologies.business_manager.modules.finance.accounting.support.AccountingSignRules.DEBIT_NORMAL;

@Service
@RequiredArgsConstructor
public class CorporateTaxService {

    private final CorporateTaxFilingRepository filingRepository;
    private final CorporateTaxPaymentRepository paymentRepository;

    private final AccountingFacade accountingFacade;
    private final AccountingAccounts accounts;

    private final TaxSystemStateService taxSystemStateService;
    private final GovernanceAuditService auditService;

    private final LedgerEntryRepository ledgerEntryRepository;
    private final AccountingPeriodRepository accountingPeriodRepository;

    private final CorporateTaxLedgerProjectionRepository projectionRepository;

    private final BranchTenantGuard branchTenantGuard;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    @Transactional(readOnly = true)
    public CorporateTaxOverviewDTO overview(UUID branchId) {

        branchTenantGuard.validate(branchId);

        UUID tenantId = tenantId();

        BigDecimal outstandingTax =
                filingRepository.totalOutstandingTax(
                        tenantId,
                        branchId
                );

        BigDecimal totalAccrued =
                filingRepository.totalAccruedTax(
                        tenantId,
                        branchId
                );

        BigDecimal totalPaid =
                paymentRepository.totalPaid(
                        tenantId,
                        branchId
                );

        var latestProjection =
                projectionRepository
                        .findByTenantIdAndBranchIdOrderByFiscalYearDescMonthNumberDesc(
                                tenantId,
                                branchId,
                                PageRequest.of(0, 1)
                        )
                        .stream()
                        .findFirst()
                        .orElse(null);

        BigDecimal estimatedTax =
                latestProjection != null
                        ? latestProjection.getEstimatedTax()
                        : BigDecimal.ZERO;

        BigDecimal taxableProfit =
                latestProjection != null
                        ? latestProjection.getTaxableProfit()
                        : BigDecimal.ZERO;

        long pendingAccruals =
                accountingPeriodRepository
                        .countByTenantIdAndBranchIdAndClosedTrueAndTaxAccruedFalse(
                                tenantId,
                                branchId
                        );

        long pendingPayments =
                filingRepository.countByTenantIdAndBranchIdAndStatus(
                        tenantId,
                        branchId,
                        CorporateTaxFilingStatus.ACCRUED
                )
                        +
                        filingRepository.countByTenantIdAndBranchIdAndStatus(
                                tenantId,
                                branchId,
                                CorporateTaxFilingStatus.PARTIALLY_PAID
                        );

        return CorporateTaxOverviewDTO
                .builder()
                .outstandingTax(outstandingTax)
                .estimatedTax(estimatedTax)
                .taxableProfit(taxableProfit)
                .pendingAccruals(pendingAccruals)
                .pendingPayments(pendingPayments)
                .totalPaidTax(totalPaid)
                .totalAccruedTax(totalAccrued)
                .build();
    }

    @Transactional(readOnly = true)
    public CorporateTaxAccrualPreviewDTO previewAccrual(
            UUID periodId,
            UUID branchId
    ) {

        branchTenantGuard.validate(branchId);

        AccountingPeriod period =
                accountingPeriodRepository
                        .findByTenantIdAndBranchIdAndId(
                                tenantId(),
                                branchId,
                                periodId
                        )
                        .orElseThrow();

        List<Object[]> movements =
                ledgerEntryRepository.netMovementByAccountType(
                        tenantId(),
                        period.getStartDate().atStartOfDay(),
                        period.getEndDate().plusDays(1).atStartOfDay(),
                        branchId,
                        DEBIT_NORMAL,
                        CREDIT_NORMAL,
                        java.util.Set.of(
                                AccountType.INCOME,
                                AccountType.EXPENSE
                        ),
                        DEBIT,
                        CREDIT
                );

        Map<AccountType, BigDecimal> totals =
                movements.stream()
                        .collect(
                                Collectors.toMap(
                                        row -> (AccountType) row[0],
                                        row -> (BigDecimal) row[1]
                                )
                        );

        BigDecimal revenue =
                totals.getOrDefault(
                        AccountType.INCOME,
                        BigDecimal.ZERO
                );

        BigDecimal expenses =
                totals.getOrDefault(
                        AccountType.EXPENSE,
                        BigDecimal.ZERO
                );

        BigDecimal profit =
                revenue.subtract(expenses);

        BigDecimal taxRate =
                taxSystemStateService
                        .getOrCreate(branchId)
                        .getCorporateTaxRate();

        BigDecimal estimatedTax =
                profit.compareTo(BigDecimal.ZERO) > 0
                        ? profit.multiply(taxRate)
                        : BigDecimal.ZERO;

        return CorporateTaxAccrualPreviewDTO
                .builder()
                .periodId(periodId)
                .startDate(period.getStartDate())
                .endDate(period.getEndDate())
                .revenue(revenue)
                .expenses(expenses)
                .taxableProfit(profit)
                .taxRate(taxRate)
                .estimatedTax(estimatedTax)
                .build();
    }

    @Transactional
    public CorporateTaxFiling accrueCorporateTax(
            UUID periodId,
            UUID branchId,
            LocalDateTime from,
            LocalDateTime to,
            String user
    ) {

        branchTenantGuard.validate(branchId);

        TaxSystemState taxState =
                taxSystemStateService.getOrCreate(branchId);

        if (taxState.getTaxMode() != BusinessTaxMode.CORPORATE) {
            throw new IllegalStateException(
                    "Corporate tax disabled"
            );
        }

        if (
                filingRepository.existsByTenantIdAndPeriod_IdAndBranchId(
                        tenantId(),
                        periodId,
                        branchId
                )
        ) {
            throw new IllegalStateException(
                    "Corporate tax already accrued"
            );
        }

        CorporateTaxAccrualPreviewDTO preview =
                previewAccrual(
                        periodId,
                        branchId
                );

        if (preview.getEstimatedTax().compareTo(BigDecimal.ZERO) <= 0) {
            throw new IllegalStateException(
                    "This period has no corporate tax liability."
            );
        }

        AccountingPeriod period =
                accountingPeriodRepository
                        .findByTenantIdAndBranchIdAndId(
                                tenantId(),
                                branchId,
                                periodId
                        )
                        .orElseThrow();

        if (
                preview.getEstimatedTax()
                        .compareTo(BigDecimal.ZERO) > 0
        ) {

            accountingFacade.post(
                    AccountingEvent.builder()
                            .eventId(UUID.randomUUID())
                            .sourceModule("CORPORATE_TAX")
                            .sourceId(periodId)
                            .reference(
                                    "CTAX-" +
                                            periodId
                            )
                            .description(
                                    "Corporate tax accrual"
                            )
                            .performedBy(user)
                            .branchId(branchId)
                            .tenantId(tenantId())
                            .accountingDate(
                                    period.getEndDate()
                            )
                            .entries(
                                    List.of(
                                            AccountingEvent.Entry.builder()
                                                    .accountId(
                                                            accounts.get(
                                                                    tenantId(),
                                                                    branchId,
                                                                    "CORPORATE_TAX_EXPENSE"
                                                            )
                                                    )
                                                    .direction(
                                                            EntryDirection.DEBIT
                                                    )
                                                    .amount(
                                                            preview.getEstimatedTax()
                                                    )
                                                    .build(),

                                            AccountingEvent.Entry.builder()
                                                    .accountId(
                                                            accounts.get(
                                                                    tenantId(),
                                                                    branchId,
                                                                    "CORPORATE_TAX_PAYABLE"
                                                            )
                                                    )
                                                    .direction(
                                                            EntryDirection.CREDIT
                                                    )
                                                    .amount(
                                                            preview.getEstimatedTax()
                                                    )
                                                    .build()
                                    )
                            )
                            .build()
            );
        }

        CorporateTaxFiling filing =
                CorporateTaxFiling.builder()
                        .tenantId(tenantId())
                        .branchId(branchId)
                        .period(period)
                        .taxableProfit(
                                preview.getTaxableProfit()
                        )
                        .taxRate(
                                preview.getTaxRate()
                        )
                        .taxAmount(
                                preview.getEstimatedTax()
                        )
                        .paidAmount(BigDecimal.ZERO)
                        .outstandingAmount(
                                preview.getEstimatedTax()
                        )
                        .status(
                                CorporateTaxFilingStatus.ACCRUED
                        )
                        .filedBy(user)
                        .filedAt(LocalDateTime.now())
                        .build();

        CorporateTaxFiling saved =
                filingRepository.save(filing);

        period.setTaxAccrued(true);

        accountingPeriodRepository.save(period);

        auditService.log(
                branchId,
                "CORPORATE_TAX_ACCRUED",
                user,
                "Tax Amount="
                        + saved.getTaxAmount()
        );

        return saved;
    }

    @Transactional
    public void recordPayment(
            UUID filingId,
            BigDecimal amount,
            UUID accountId,
            String user
    ) {

        if (
                amount == null ||
                        amount.compareTo(BigDecimal.ZERO) <= 0
        ) {
            throw new IllegalArgumentException(
                    "Payment amount must be positive"
            );
        }

        CorporateTaxFiling filing =
                filingRepository
                        .findByTenantIdAndId(
                                tenantId(),
                                filingId
                        )
                        .orElseThrow();

        branchTenantGuard.validate(
                filing.getBranchId()
        );

        if (
                amount.compareTo(
                        filing.getOutstandingAmount()
                ) > 0
        ) {
            throw new IllegalStateException(
                    "Payment exceeds outstanding balance"
            );
        }

        accountingFacade.post(
                AccountingEvent.builder()
                        .eventId(UUID.randomUUID())
                        .sourceModule(
                                "CORPORATE_TAX_PAYMENT"
                        )
                        .sourceId(filingId)
                        .reference(
                                "CTAX-PAY-" + filingId
                        )
                        .description(
                                "Corporate tax payment"
                        )
                        .performedBy(user)
                        .branchId(
                                filing.getBranchId()
                        )
                        .tenantId(
                                tenantId()
                        )
                        .entries(
                                List.of(
                                        AccountingEvent.Entry.builder()
                                                .accountId(
                                                        accounts.get(
                                                                tenantId(),
                                                                filing.getBranchId(),"CORPORATE_TAX_PAYABLE"
                                                        )
                                                )
                                                .direction(
                                                        EntryDirection.DEBIT
                                                )
                                                .amount(
                                                        amount
                                                )
                                                .build(),

                                        AccountingEvent.Entry.builder()
                                                .accountId(
                                                        accountId
                                                )
                                                .direction(
                                                        EntryDirection.CREDIT
                                                )
                                                .amount(
                                                        amount
                                                )
                                                .build()
                                )
                        )
                        .build()
        );

        CorporateTaxPayment payment =
                CorporateTaxPayment.builder()
                        .tenantId(
                                tenantId()
                        )
                        .branchId(
                                filing.getBranchId()
                        )
                        .filing(
                                filing
                        )
                        .amount(
                                amount
                        )
                        .fundingAccountId(
                                accountId
                        )
                        .recordedBy(
                                user
                        )
                        .recordedAt(
                                LocalDateTime.now()
                        )
                        .build();

        paymentRepository.save(
                payment
        );

        filing.setPaidAmount(
                filing.getPaidAmount()
                        .add(amount)
        );

        filing.setOutstandingAmount(
                filing.getOutstandingAmount()
                        .subtract(amount)
        );

        if (
                filing.getOutstandingAmount()
                        .compareTo(BigDecimal.ZERO) == 0
        ) {
            filing.setStatus(
                    CorporateTaxFilingStatus.PAID
            );

            filing.setPaidAt(
                    LocalDateTime.now()
            );
        } else {
            filing.setStatus(
                    CorporateTaxFilingStatus.PARTIALLY_PAID
            );
        }

        filingRepository.save(
                filing
        );

        auditService.log(
                filing.getBranchId(),
                "CORPORATE_TAX_PAYMENT",
                user,
                "Amount=" + amount
        );
    }
}