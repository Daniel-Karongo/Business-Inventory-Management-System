package com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.service;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.domain.PurchaseInvoice;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.enums.PurchaseInvoicePostingStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.enums.PurchaseInvoiceStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.repository.PurchaseInvoiceRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.shared.validation.ApValidationService;
import com.IntegrityTechnologies.business_manager.modules.finance.shared.enums.FinancialDocumentStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.shared.enums.FinancialPostingStatus;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class PurchaseInvoicePostingService {

    private final PurchaseInvoiceRepository
            repository;

    private final PurchaseInvoiceService
            invoiceService;

    private final ApValidationService
            validationService;

    @Transactional
    public PurchaseInvoice post(
            UUID branchId,
            UUID invoiceId
    ) {

        PurchaseInvoice invoice =
                invoiceService.getManaged(
                        branchId,
                        invoiceId
                );

        validationService
                .validatePostingAllowed(
                        invoice
                );

        if (
                invoice.getStatus()
                        != PurchaseInvoiceStatus.APPROVED
        ) {
            throw new IllegalStateException(
                    "Only approved invoices may be posted"
            );
        }

        /*
         * PROCUREMENT ACCOUNTING MODEL:
         *
         * GOODS RECEIPT:
         *
         * DR Inventory
         * CR GRNI
         *
         * INVOICE MATCH:
         *
         * DR GRNI
         * DR VAT Input
         * CR Accounts Payable
         *
         * Therefore invoice posting here is:
         * operational + subledger lifecycle posting only.
         *
         * Financial liability recognition occurs during
         * GRNI matching.
         */

        invoice.setPostingLifecycleStatus(
                PurchaseInvoicePostingStatus.POSTED
        );

        invoice.setPostingStatus(
                FinancialPostingStatus.POSTED
        );

        invoice.setStatus(
                PurchaseInvoiceStatus.POSTED
        );

        invoice.setDocumentStatus(
                FinancialDocumentStatus.ACTIVE
        );

        invoice.setPosted(true);

        invoice.setPostedAt(
                LocalDateTime.now()
        );

        invoice.setPostedBy(
                currentUser()
        );

        return repository.save(invoice);
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
}