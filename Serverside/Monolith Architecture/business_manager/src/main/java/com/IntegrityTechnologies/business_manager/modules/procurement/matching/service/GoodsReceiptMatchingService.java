package com.IntegrityTechnologies.business_manager.modules.procurement.matching.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.domain.PurchaseInvoice;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.service.PurchaseInvoiceService;
import com.IntegrityTechnologies.business_manager.modules.procurement.matching.dto.MatchInvoiceToReceiptsRequest;
import com.IntegrityTechnologies.business_manager.modules.procurement.receipt.domain.GoodsReceipt;
import com.IntegrityTechnologies.business_manager.modules.procurement.receipt.repository.GoodsReceiptRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static java.math.RoundingMode.HALF_UP;

@Service
@RequiredArgsConstructor
public class GoodsReceiptMatchingService {

    private final GoodsReceiptRepository
            goodsReceiptRepository;

    private final PurchaseInvoiceService
            invoiceService;

    private final AccountingFacade
            accountingFacade;

    private final AccountingAccounts
            accountingAccounts;

    private final BranchTenantGuard
            branchTenantGuard;

    @Transactional
    public void match(
            MatchInvoiceToReceiptsRequest request
    ) {

        branchTenantGuard.validate(
                request.getBranchId()
        );

        UUID tenantId =
                TenantContext.getTenantId();

        PurchaseInvoice invoice =
                invoiceService.getManaged(
                        request.getBranchId(),
                        request.getPurchaseInvoiceId()
                );

        List<GoodsReceipt> receipts =
                new ArrayList<>();

        BigDecimal receiptInventoryTotal =
                BigDecimal.ZERO;

        for (
                UUID receiptId :
                request.getGoodsReceiptIds()
        ) {

            GoodsReceipt receipt =
                    goodsReceiptRepository
                            .findByTenantIdAndBranchIdAndId(
                                    tenantId,
                                    request.getBranchId(),
                                    receiptId
                            )
                            .orElseThrow(() ->
                                    new IllegalArgumentException(
                                            "Goods receipt not found"
                                    )
                            );

            if (
                    !receipt.getTenantId()
                            .equals(tenantId)
            ) {
                throw new IllegalStateException(
                        "Cross-tenant receipt access denied"
                );
            }

            if (!receipt.getBranchId().equals(request.getBranchId())) {
                throw new IllegalStateException(
                        "Cross-branch receipt access denied"
                );
            }

            if (receipt.isInvoiced()) {
                continue;
            }

            if (
                    !receipt.getSupplierId()
                            .equals(
                                    invoice.getSupplier().getId()
                            )
            ) {
                throw new IllegalStateException(
                        "Supplier mismatch between invoice and receipt"
                );
            }

            receiptInventoryTotal =
                    receiptInventoryTotal.add(
                            receipt.getNetAmount() != null
                                    ? receipt.getNetAmount()
                                    : BigDecimal.ZERO
                    );

            receipts.add(receipt);
        }

        BigDecimal variance =
                receiptInventoryTotal
                        .setScale(2, HALF_UP)
                        .subtract(
                                invoice.getSubtotal()
                                        .setScale(2, HALF_UP)
                        )
                        .abs();

        if (variance.compareTo(new BigDecimal("0.01")) > 0) {
            throw new IllegalStateException(
                    "Invoice subtotal does not match GRNI receipt total"
            );
        }

        /*
         * GRNI CLEARING ENTRY
         *
         * DR GRNI
         * DR VAT INPUT
         * CR ACCOUNTS PAYABLE
         */

        List<AccountingEvent.Entry> entries =
                new ArrayList<>();

        entries.add(
                AccountingEvent.Entry.builder()
                        .accountId(
                                accountingAccounts.get(
                                        tenantId,
                                        request.getBranchId(),
                                        "GOODS_RECEIVED_NOT_INVOICED"
                                )
                        )
                        .direction(
                                EntryDirection.DEBIT
                        )
                        .amount(
                                receiptInventoryTotal
                        )
                        .build()
        );

        if (
                invoice.getVatAmount()
                        .compareTo(BigDecimal.ZERO) > 0
        ) {

            entries.add(
                    AccountingEvent.Entry.builder()
                            .accountId(
                                    accountingAccounts.get(
                                            tenantId,
                                            request.getBranchId(),
                                            "VAT_INPUT"
                                    )
                            )
                            .direction(
                                    EntryDirection.DEBIT
                            )
                            .amount(
                                    invoice.getVatAmount()
                            )
                            .build()
            );
        }

        entries.add(
                AccountingEvent.Entry.builder()
                        .accountId(
                                accountingAccounts.get(
                                        tenantId,
                                        request.getBranchId(),
                                        "ACCOUNTS_PAYABLE"
                                )
                        )
                        .direction(
                                EntryDirection.CREDIT
                        )
                        .amount(
                                receiptInventoryTotal.add(
                                        invoice.getVatAmount()
                                )
                        )
                        .build()
        );

        accountingFacade.post(
                AccountingEvent.builder()
                        .eventId(
                                UUID.nameUUIDFromBytes(
                                        (
                                                "GRNI_MATCH:" +
                                                        invoice.getId()
                                        ).getBytes()
                                )
                        )
                        .tenantId(tenantId)
                        .branchId(request.getBranchId())
                        .sourceModule(
                                "PURCHASE_INVOICE_MATCHING"
                        )
                        .sourceId(invoice.getId())
                        .reference(
                                invoice.getDocumentNumber()
                        )
                        .description(
                                "GRNI clearing against supplier invoice"
                        )
                        .performedBy("SYSTEM")
                        .entries(entries)
                        .build()
        );

        for (GoodsReceipt receipt : receipts) {

            if (receipt.isInvoiced()) {
                continue;
            }

            receipt.setInvoiced(true);
            receipt.setMatchedInvoiceId(invoice.getId());
            receipt.setInvoicedAt(LocalDateTime.now());
        }

        goodsReceiptRepository.saveAll(receipts);
    }
}