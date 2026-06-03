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
import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.model.CorporateTaxFiling;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.repository.CorporateTaxFilingRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static com.IntegrityTechnologies.business_manager.modules.finance.accounting.support.AccountingSignRules.*;

@Service
@RequiredArgsConstructor
public class CorporateTaxService {

    private final CorporateTaxFilingRepository filingRepository;
    private final AccountingFacade accountingFacade;
    private final AccountingAccounts accounts;
    private final TaxSystemStateService taxSystemStateService;
    private final GovernanceAuditService auditService;
    private final LedgerEntryRepository ledgerEntryRepository;
    private final BranchTenantGuard branchTenantGuard;
    private final AccountingPeriodRepository accountingPeriodRepository;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    @Transactional
    public CorporateTaxFiling accrueCorporateTax(
            UUID periodId,
            UUID branchId,
            LocalDateTime from,
            LocalDateTime to,
            String user
    ) {

        if (branchId == null) {
            throw new IllegalArgumentException("branchId is required for corporate tax accrual");
        }

        branchTenantGuard.validate(branchId);

        TaxSystemState taxState =
                taxSystemStateService.getOrCreate(branchId);

        if (taxState.getTaxMode() != BusinessTaxMode.CORPORATE) {
            throw new IllegalStateException(
                    "Corporate tax accrual disabled. Business tax mode is "
                            + taxState.getTaxMode()
            );
        }

        if (filingRepository.existsByTenantIdAndPeriod_IdAndBranchId(
                tenantId(),
                periodId,
                branchId
        )) {
            throw new IllegalStateException(
                    "Corporate tax already accrued for this period."
            );
        }

        AccountingPeriod period =
                accountingPeriodRepository
                        .findByTenantIdAndBranchIdAndId(
                                tenantId(),
                                branchId,
                                periodId
                        )
                        .orElseThrow(() ->
                                new IllegalArgumentException(
                                        "Accounting period not found"
                                ));

        List<Object[]> movements =
                ledgerEntryRepository.netMovementByAccountType(
                        tenantId(),
                        from,
                        to,
                        branchId,
                        DEBIT_NORMAL,
                        CREDIT_NORMAL,
                        Set.of(
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
                revenue.subtract(
                        expenses
                );

        BigDecimal taxRate =
                taxSystemStateService
                        .getOrCreate(branchId)
                        .getCorporateTaxRate();

        BigDecimal taxAmount =
                BigDecimal.ZERO;

        if (profit.compareTo(BigDecimal.ZERO) > 0) {

            taxAmount =
                    profit.multiply(
                            taxRate
                    );

            accountingFacade.post(
                    AccountingEvent.builder()
                            .eventId(UUID.randomUUID())
                            .sourceModule("CORPORATE_TAX")
                            .sourceId(periodId)
                            .reference("CTAX-" + periodId + "-" + branchId)
                            .description("Corporate tax accrual")
                            .performedBy(user)
                            .branchId(branchId)
                            .tenantId(tenantId())
                            .accountingDate(period.getEndDate())
                            .entries(
                                    List.of(
                                            AccountingEvent.Entry.builder()
                                                    .accountId(accounts.get(
                                                            tenantId(),
                                                            branchId,
                                                            "CORPORATE_TAX_EXPENSE"
                                                    ))
                                                    .direction(EntryDirection.DEBIT)
                                                    .amount(taxAmount)
                                                    .build(),

                                            AccountingEvent.Entry.builder()
                                                    .accountId(accounts.get(
                                                            tenantId(),
                                                            branchId,
                                                            "CORPORATE_TAX_PAYABLE"
                                                    ))
                                                    .direction(EntryDirection.CREDIT)
                                                    .amount(taxAmount)
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
                        .taxableProfit(profit)
                        .taxRate(taxRate)
                        .taxAmount(taxAmount)
                        .filedBy(user)
                        .filedAt(LocalDateTime.now())
                        .paid(false)
                        .build();

        CorporateTaxFiling saved =
                filingRepository.save(
                        filing
                );

        auditService.log(
                branchId,
                "CORPORATE_TAX_ACCRUED",
                user,
                "Period=" + periodId +
                        ", taxableProfit=" + profit +
                        ", taxAmount=" + taxAmount
        );

        return saved;
    }

    @Transactional
    public void markPaid(
            UUID filingId,
            String user,
            UUID paymentAccount
    ) {

        UUID branchId =
                filingRepository
                        .findByTenantIdAndId(
                                tenantId(),
                                filingId
                        )
                        .orElseThrow(() ->
                                new IllegalArgumentException(
                                        "Corporate tax filing not found"
                                ))
                        .getBranchId();

        TaxSystemState taxState =
                taxSystemStateService.getOrCreate(branchId);

        if (taxState.getTaxMode() != BusinessTaxMode.CORPORATE) {
            throw new IllegalStateException(
                    "Corporate tax payment disabled. Business tax mode is "
                            + taxState.getTaxMode()
            );
        }

        CorporateTaxFiling filing =
                filingRepository
                        .findByTenantIdAndId(
                                tenantId(),
                                filingId
                        )
                        .orElseThrow(() ->
                                new IllegalArgumentException(
                                        "Corporate tax filing not found"
                                ));

        if (filing.isPaid()) {
            return;
        }

        branchTenantGuard.validate(
                branchId
        );

        accountingFacade.post(
                AccountingEvent.builder()
                        .eventId(UUID.randomUUID())
                        .sourceModule("CORPORATE_TAX_PAYMENT")
                        .sourceId(filingId)
                        .reference("CTAX-PAY-" + filingId)
                        .description("Corporate tax payment")
                        .performedBy(user)
                        .branchId(branchId)
                        .tenantId(tenantId())
                        .entries(
                                List.of(
                                        AccountingEvent.Entry.builder()
                                                .accountId(
                                                        accounts.get(
                                                                tenantId(),
                                                                branchId,"CORPORATE_TAX_PAYABLE"
                                                        )
                                                )
                                                .direction(EntryDirection.DEBIT)
                                                .amount(filing.getTaxAmount())
                                                .build(),

                                        AccountingEvent.Entry.builder()
                                                .accountId(paymentAccount)
                                                .direction(EntryDirection.CREDIT)
                                                .amount(filing.getTaxAmount())
                                                .build()
                                )
                        )
                        .build()
        );

        filing.setPaid(true);
        filing.setPaidAt(LocalDateTime.now());

        filingRepository.save(
                filing
        );

        auditService.log(
                branchId,
                "CORPORATE_TAX_PAID",
                user,
                "FilingId=" + filingId +
                        ", amount=" + filing.getTaxAmount()
        );
    }

    public List<CorporateTaxFiling> listAll() {
        return filingRepository.findAll();
    }
}