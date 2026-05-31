package com.IntegrityTechnologies.business_manager.modules.finance.sales.base.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.model.SaleLineItem;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class SalesVatAccountingService {

    private final AccountingFacade accountingFacade;
    private final AccountingAccounts accountingAccounts;

    public void postSaleAccounting(
            UUID tenantId,
            UUID branchId,
            Sale sale,
            String performedBy
    ) {

        BigDecimal totalNet = BigDecimal.ZERO;
        BigDecimal totalVat = BigDecimal.ZERO;

        for (SaleLineItem li : sale.getLineItems()) {

            totalNet = totalNet.add(
                    defaultZero(li.getNetAmount())
            );

            totalVat = totalVat.add(
                    defaultZero(li.getVatAmount())
            );
        }

        BigDecimal gross =
                totalNet.add(totalVat);

        accountingFacade.post(

                AccountingEvent.builder()
                        .eventId(UUID.randomUUID())
                        .tenantId(tenantId)
                        .branchId(branchId)
                        .sourceModule("SALE")
                        .sourceId(sale.getId())
                        .reference(sale.getReceiptNo())
                        .description("Sale revenue with VAT")
                        .performedBy(performedBy)

                        .entries(List.of(

                                AccountingEvent.Entry.builder()
                                        .accountId(
                                                accountingAccounts.get(
                                                        tenantId,
                                                        branchId,
                                                        "ACCOUNTS_RECEIVABLE"
                                                )
                                        )
                                        .direction(EntryDirection.DEBIT)
                                        .amount(gross)
                                        .build(),

                                AccountingEvent.Entry.builder()
                                        .accountId(
                                                accountingAccounts.get(
                                                        tenantId,
                                                        branchId,
                                                        "REVENUE"
                                                )
                                        )
                                        .direction(EntryDirection.CREDIT)
                                        .amount(totalNet)
                                        .build(),

                                AccountingEvent.Entry.builder()
                                        .accountId(
                                                accountingAccounts.get(
                                                        tenantId,
                                                        branchId,
                                                        "VAT_OUTPUT"
                                                )
                                        )
                                        .direction(EntryDirection.CREDIT)
                                        .amount(totalVat)
                                        .build()

                        ))

                        .build()
        );
    }

    private BigDecimal defaultZero(BigDecimal value) {
        return value != null
                ? value
                : BigDecimal.ZERO;
    }
}