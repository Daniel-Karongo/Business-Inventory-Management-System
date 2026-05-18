package com.IntegrityTechnologies.business_manager.modules.finance.ap.reporting.service;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.domain.PurchaseInvoice;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.repository.PurchaseInvoiceRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.reporting.dto.AccountsPayableSummaryRowDto;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.*;

@Service
@RequiredArgsConstructor
public class AccountsPayableReportingService {

    private final PurchaseInvoiceRepository
            invoiceRepository;

    private final BranchTenantGuard
            branchTenantGuard;

    public List<AccountsPayableSummaryRowDto>
    summary(
            UUID tenantId,
            UUID branchId
    ) {

        branchTenantGuard.validate(
                branchId
        );

        List<PurchaseInvoice> invoices =
                invoiceRepository
                        .findByTenantIdAndBranchId(
                                tenantId,
                                branchId
                        );

        Map<UUID, AccountsPayableSummaryRowDto>
                map =
                new LinkedHashMap<>();

        for (
                PurchaseInvoice invoice :
                invoices
        ) {

            if (
                    invoice.getOutstandingAmount()
                            .compareTo(BigDecimal.ZERO) <= 0
            ) {
                continue;
            }

            UUID supplierId =
                    invoice.getSupplier().getId();

            AccountsPayableSummaryRowDto row =
                    map.computeIfAbsent(
                            supplierId,
                            id -> AccountsPayableSummaryRowDto
                                    .builder()
                                    .supplierId(
                                            supplierId
                                    )
                                    .supplierName(
                                            invoice.getSupplier().getName()
                                    )
                                    .openInvoiceCount(0L)
                                    .overdueInvoiceCount(0L)
                                    .totalOutstanding(BigDecimal.ZERO)
                                    .unappliedPayments(BigDecimal.ZERO)
                                    .netExposure(BigDecimal.ZERO)
                                    .build()
                    );

            row.setOpenInvoiceCount(
                    row.getOpenInvoiceCount() + 1
            );

            row.setTotalOutstanding(
                    row.getTotalOutstanding()
                            .add(
                                    invoice.getOutstandingAmount()
                            )
            );

            row.setNetExposure(
                    row.getTotalOutstanding()
            );

            if (
                    invoice.isOverdue()
            ) {
                row.setOverdueInvoiceCount(
                        row.getOverdueInvoiceCount() + 1
                );
            }
        }

        return new ArrayList<>(
                map.values()
        );
    }
}