package com.IntegrityTechnologies.business_manager.modules.finance.tax.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountRole;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.GovernanceAuditService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.LedgerEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.service.PeriodGuardService;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.config.TaxProperties;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.CorporateTaxFiling;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.enums.BusinessTaxMode;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.CorporateTaxFilingRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.CorporateTaxLedgerProjectionRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class CorporateTaxService {

    private final LedgerEntryRepository ledgerRepository;
    private final CorporateTaxFilingRepository filingRepository;
    private final AccountingFacade accountingFacade;
    private final AccountingAccounts accounts;
    private final TaxProperties taxProperties;
    private final PeriodGuardService periodGuardService;
    private final GovernanceAuditService auditService;
    private final CorporateTaxLedgerProjectionRepository projectionRepository;
    private final BranchTenantGuard branchTenantGuard;

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

        if (taxProperties.getBusinessTaxMode() != BusinessTaxMode.CORPORATE) {
            throw new IllegalStateException(
                    "Corporate tax accrual disabled. Business tax mode is "
                            + taxProperties.getBusinessTaxMode()
            );
        }

        periodGuardService.validateOpenPeriod(from.toLocalDate(), branchId);

        if (filingRepository.existsByTenantIdAndPeriodIdAndBranchId(
                tenantId(),
                periodId,
                branchId
        )) {

            throw new IllegalStateException(
                    "Corporate tax already accrued for this period."
            );
        }

        var projection =
                projectionRepository
                        .findByTenantIdAndBranchIdAndFiscalYearAndMonthNumber(
                                tenantId(),
                                branchId,
                                from.getYear(),
                                from.getMonthValue()
                        )
                        .orElseThrow(() ->
                                new IllegalStateException("Tax projection missing"));

        BigDecimal profit = projection.getTaxableProfit();

        BigDecimal taxRate = taxProperties.getCorporateTaxRate();
        BigDecimal taxAmount = BigDecimal.ZERO;

        if (profit.compareTo(BigDecimal.ZERO) > 0) {
            taxAmount = profit.multiply(taxRate);

            accountingFacade.post(
                    AccountingEvent.builder()
                            .eventId(UUID.randomUUID())
                            .sourceModule("CORPORATE_TAX")
                            .sourceId(periodId)
                            .reference("CTAX-" + periodId + "-" + branchId)
                            .description("Corporate tax accrual")
                            .performedBy(user)
                            .branchId(branchId)
                            .entries(
                                    List.of(
                                            AccountingEvent.Entry.builder()
                                                    .accountId(accounts.get(tenantId(), branchId, AccountRole.CORPORATE_TAX_EXPENSE))
                                                    .direction(EntryDirection.DEBIT)
                                                    .amount(taxAmount)
                                                    .build(),

                                            AccountingEvent.Entry.builder()
                                                    .accountId(accounts.get(tenantId(), branchId, AccountRole.CORPORATE_TAX_PAYABLE))
                                                    .direction(EntryDirection.CREDIT)
                                                    .amount(taxAmount)
                                                    .build()
                                    )
                            )
                            .build()
            );
        }

        CorporateTaxFiling filing = CorporateTaxFiling.builder()
                .tenantId(tenantId())
                .branchId(branchId)
                .periodId(periodId)
                .taxableProfit(profit)
                .taxRate(taxRate)
                .taxAmount(taxAmount)
                .filedBy(user)
                .filedAt(LocalDateTime.now())
                .paid(false)
                .build();

        CorporateTaxFiling saved = filingRepository.save(filing);
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
    public void markPaid(UUID filingId, String user, UUID paymentAccount) {

        if (taxProperties.getBusinessTaxMode() != BusinessTaxMode.CORPORATE) {
            throw new IllegalStateException(
                    "Corporate tax payment disabled. Business tax mode is "
                            + taxProperties.getBusinessTaxMode()
            );
        }

        CorporateTaxFiling filing =
                filingRepository.findByTenantIdAndId(
                                tenantId(),
                                filingId
                        )
                        .orElseThrow(() ->
                                new IllegalArgumentException("Corporate tax filing not found"));

        if (filing.isPaid()) return;

        UUID branchId = filing.getBranchId();
        branchTenantGuard.validate(branchId);

        accountingFacade.post(
                AccountingEvent.builder()
                        .eventId(UUID.randomUUID())
                        .sourceModule("CORPORATE_TAX_PAYMENT")
                        .sourceId(filingId)
                        .reference("CTAX-PAY-" + filingId)
                        .description("Corporate tax payment")
                        .performedBy(user)
                        .branchId(branchId)   // ✅ CRITICAL FIX
                        .entries(
                                List.of(
                                        AccountingEvent.Entry.builder()
                                                .accountId(accounts.get(tenantId(),
                                                        branchId, AccountRole.CORPORATE_TAX_PAYABLE))
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
        filingRepository.save(filing);
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