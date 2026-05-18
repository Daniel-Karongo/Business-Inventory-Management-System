package com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.service;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.domain.PurchaseInvoice;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.enums.PurchaseInvoicePostingStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.enums.PurchaseInvoiceStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.repository.PurchaseInvoiceRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.shared.enums.FinancialDocumentStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.shared.enums.FinancialPostingStatus;
import com.IntegrityTechnologies.business_manager.modules.procurement.receipt.domain.GoodsReceipt;
import com.IntegrityTechnologies.business_manager.modules.procurement.receipt.repository.GoodsReceiptRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class PurchaseInvoiceReversalService {

    private final PurchaseInvoiceRepository
            repository;

    private final PurchaseInvoiceService
            invoiceService;

    private final GoodsReceiptRepository
            goodsReceiptRepository;

    @Transactional
    public PurchaseInvoice reverse(
            UUID branchId,
            UUID invoiceId,
            String reason,
            String reversedBy
    ) {

        PurchaseInvoice invoice =
                invoiceService.getManaged(
                        branchId,
                        invoiceId
                );

        if (invoice.isReversed()) {
            throw new IllegalStateException(
                    "Invoice already reversed"
            );
        }

        /*
         * FULL OPEN-ITEM PROTECTION
         */

        if (
                invoice.getOutstandingAmount()
                        .compareTo(
                                invoice.getTotalAmount()
                        ) != 0
        ) {
            throw new IllegalStateException(
                    "Cannot reverse allocated invoice"
            );
        }

        /*
         * UNMATCH GOODS RECEIPTS
         */

        List<GoodsReceipt> receipts =
                goodsReceiptRepository
                        .findByTenantIdAndBranchIdAndMatchedInvoiceId(
                                invoice.getTenantId(),
                                branchId,
                                invoice.getId()
                        );

        for (
                GoodsReceipt receipt :
                receipts
        ) {

            receipt.setInvoiced(false);

            receipt.setMatchedInvoiceId(null);

            receipt.setInvoicedAt(null);

            goodsReceiptRepository.save(receipt);
        }

        /*
         * REVERSE DOCUMENT
         */

        invoice.setReversed(true);

        invoice.setReversedAt(
                LocalDateTime.now()
        );

        invoice.setReversedBy(
                reversedBy
        );

        invoice.setReversalReason(
                reason
        );

        invoice.setStatus(
                PurchaseInvoiceStatus.REVERSED
        );

        invoice.setDocumentStatus(
                FinancialDocumentStatus.REVERSED
        );

        invoice.setPostingLifecycleStatus(
                PurchaseInvoicePostingStatus.REVERSED
        );

        invoice.setPostingStatus(
                FinancialPostingStatus.REVERSED
        );

        return repository.save(invoice);
    }
}