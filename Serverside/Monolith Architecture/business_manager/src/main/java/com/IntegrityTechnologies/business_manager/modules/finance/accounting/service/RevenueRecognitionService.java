package com.IntegrityTechnologies.business_manager.modules.finance.accounting.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountRole;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.RevenueRecognitionMode;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.model.PaymentStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.model.SaleLineItem;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.service.SalesVatAccountingService;
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
    private final SalesVatAccountingService salesVatAccountingService;

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

        salesVatAccountingService.postSaleAccounting(
                TenantContext.getTenantId(),
                branchId,
                sale,
                sale.getCreatedBy()
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