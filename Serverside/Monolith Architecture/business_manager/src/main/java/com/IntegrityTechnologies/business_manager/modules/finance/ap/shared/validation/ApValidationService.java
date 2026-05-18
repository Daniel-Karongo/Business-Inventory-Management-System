package com.IntegrityTechnologies.business_manager.modules.finance.ap.shared.validation;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.domain.PurchaseInvoice;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.enums.PurchaseInvoicePostingStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.enums.PurchaseInvoiceStatus;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDate;

@Service
public class ApValidationService {

    public void validateDraftMutationAllowed(
            PurchaseInvoice invoice
    ) {

        if (invoice.isCancelled()) {
            throw new IllegalStateException(
                    "Cancelled invoice cannot be modified"
            );
        }

        if (invoice.isReversed()) {
            throw new IllegalStateException(
                    "Reversed invoice cannot be modified"
            );
        }

        if (
                invoice.getPostingLifecycleStatus()
                        == PurchaseInvoicePostingStatus.POSTED
        ) {
            throw new IllegalStateException(
                    "Posted invoice is immutable"
            );
        }

        if (
                invoice.getStatus()
                        != PurchaseInvoiceStatus.DRAFT
        ) {
            throw new IllegalStateException(
                    "Only draft invoices can be modified"
            );
        }
    }

    public void validatePostingAllowed(
            PurchaseInvoice invoice
    ) {

        if (invoice.isCancelled()) {
            throw new IllegalStateException(
                    "Cancelled invoice cannot be posted"
            );
        }

        if (invoice.isReversed()) {
            throw new IllegalStateException(
                    "Reversed invoice cannot be reposted"
            );
        }

        if (
                invoice.getPostingLifecycleStatus()
                        == PurchaseInvoicePostingStatus.POSTED
        ) {
            throw new IllegalStateException(
                    "Invoice already posted"
            );
        }

        if (
                invoice.getTotalAmount() == null
                        || invoice.getTotalAmount()
                        .compareTo(BigDecimal.ZERO) <= 0
        ) {
            throw new IllegalStateException(
                    "Invoice total must be greater than zero"
            );
        }

        if (
                invoice.getDueDate()
                        .isBefore(invoice.getDocumentDate())
        ) {
            throw new IllegalStateException(
                    "Due date cannot be before invoice date"
            );
        }

        if (
                invoice.getLines() == null
                        || invoice.getLines().isEmpty()
        ) {
            throw new IllegalStateException(
                    "Invoice must contain at least one line"
            );
        }
    }

    public void validateAllocationAmount(
            BigDecimal invoiceOutstanding,
            BigDecimal paymentUnapplied,
            BigDecimal allocationAmount
    ) {

        if (
                allocationAmount == null
                        || allocationAmount.compareTo(BigDecimal.ZERO) <= 0
        ) {
            throw new IllegalStateException(
                    "Allocation amount must be greater than zero"
            );
        }

        if (
                allocationAmount.compareTo(invoiceOutstanding) > 0
        ) {
            throw new IllegalStateException(
                    "Allocation exceeds invoice outstanding balance"
            );
        }

        if (
                allocationAmount.compareTo(paymentUnapplied) > 0
        ) {
            throw new IllegalStateException(
                    "Allocation exceeds unapplied payment balance"
            );
        }
    }

    public boolean isOverdue(
            PurchaseInvoice invoice
    ) {

        return invoice.getOutstandingAmount()
                .compareTo(BigDecimal.ZERO) > 0
                &&
                invoice.getDueDate()
                        .isBefore(LocalDate.now());
    }
}