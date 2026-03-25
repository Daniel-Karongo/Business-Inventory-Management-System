package com.IntegrityTechnologies.business_manager.modules.finance.accounting.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountRole;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.RevenueRecognitionMode;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.model.PaymentStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.model.SaleLineItem;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class RevenueRecognitionService {

    private final AccountingFacade accountingFacade;
    private final AccountingAccounts accounts;
    private final BranchAccountingSettingsService branchAccountingSettingsService;
    private final BranchTenantGuard branchTenantGuard;

    @Transactional
    public void recognizeIfEligible(Sale sale) {

        UUID branchId = sale.getLineItems().stream()
                .map(SaleLineItem::getBranchId)
                .findFirst()
                .orElseThrow(() -> new IllegalStateException("Sale has no branch"));

        branchTenantGuard.validate(branchId);

        RevenueRecognitionMode mode =
                branchAccountingSettingsService.getMode(branchId);

        if (mode == RevenueRecognitionMode.DELIVERY
                && sale.getStatus() != Sale.SaleStatus.COMPLETED) {
            return;
        }

        if (mode == RevenueRecognitionMode.PAYMENT
                && !isFullyPaid(sale)) {
            return;
        }

        if (sale.getStatus() == Sale.SaleStatus.REFUNDED
                || sale.getStatus() == Sale.SaleStatus.CANCELLED) {
            return;
        }

        if (accountingFacade.isAlreadyPosted("SALE", sale.getId())) {
            return;
        }

        BigDecimal totalNet = sale.getLineItems().stream()
                .map(SaleLineItem::getNetAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        BigDecimal totalVat = sale.getLineItems().stream()
                .map(SaleLineItem::getVatAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        accountingFacade.post(
                AccountingEvent.builder()
                        .eventId(UUID.randomUUID())
                        .sourceModule("SALE")
                        .sourceId(sale.getId())
                        .reference(sale.getReceiptNo())
                        .description("Revenue recognition")
                        .performedBy(sale.getCreatedBy())
                        .branchId(branchId)
                        .accountingDate(
                                sale.getCreatedAt() != null
                                        ? sale.getCreatedAt().toLocalDate()
                                        : LocalDate.now()
                        )
                        .entries(List.of(
                                AccountingEvent.Entry.builder()
                                        .accountId(accounts.get(TenantContext.getTenantId(), branchId, AccountRole.ACCOUNTS_RECEIVABLE))
                                        .direction(EntryDirection.DEBIT)
                                        .amount(totalNet.add(totalVat))
                                        .build(),
                                AccountingEvent.Entry.builder()
                                        .accountId(accounts.get(TenantContext.getTenantId(), branchId, AccountRole.REVENUE))
                                        .direction(EntryDirection.CREDIT)
                                        .amount(totalNet)
                                        .build(),
                                AccountingEvent.Entry.builder()
                                        .accountId(accounts.get(TenantContext.getTenantId(), branchId, AccountRole.VAT_OUTPUT))
                                        .direction(EntryDirection.CREDIT)
                                        .amount(totalVat)
                                        .build()
                        ))
                        .build()
        );
    }

    private boolean isFullyPaid(Sale sale) {
        BigDecimal totalPaid = sale.getPayments().stream()
                .filter(p -> p.getStatus() == PaymentStatus.SUCCESS)
                .map(p -> p.getAmount())
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        return totalPaid.compareTo(sale.getTotalAmount()) >= 0;
    }
}