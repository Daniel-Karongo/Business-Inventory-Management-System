package com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.service;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.domain.SupplierPaymentAllocation;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.dto.*;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.enums.ApAllocationType;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.enums.PaymentAllocationStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.repository.SupplierPaymentAllocationRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.domain.PurchaseInvoice;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.enums.PurchaseInvoiceStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.repository.PurchaseInvoiceRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.service.PurchaseInvoiceService;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.enums.SupplierPaymentStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.service.SupplierPaymentService;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.shared.validation.ApValidationService;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.model.SupplierPayment;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.repository.SupplierPaymentRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.shared.enums.FinancialPostingStatus;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class ApAllocationService {

    private final SupplierPaymentAllocationRepository allocationRepository;
    private final SupplierPaymentRepository paymentRepository;
    private final PurchaseInvoiceService invoiceService;
    private final ApValidationService validationService;
    private final BranchTenantGuard branchTenantGuard;
    private final PurchaseInvoiceRepository invoiceRepository;
    private final SupplierPaymentService paymentService;

    @Transactional
    public AllocationResponse allocate(
            AllocateSupplierPaymentRequest request
    ) {
        try {

            branchTenantGuard.validate(
                    request.getBranchId()
            );

            UUID tenantId =
                    TenantContext.getTenantId();

            SupplierPayment payment =
                    paymentRepository
                            .findLockedByTenantIdAndBranchIdAndId(
                                    tenantId,
                                    request.getBranchId(),
                                    request.getPaymentId()
                            )
                            .orElseThrow(() ->
                                    new IllegalArgumentException(
                                            "Supplier payment not found"
                                    )
                            );

            PurchaseInvoice invoice =
                    invoiceRepository
                            .findLockedByTenantIdAndBranchIdAndId(
                                    tenantId,
                                    request.getBranchId(),
                                    request.getPurchaseInvoiceId()
                            ).orElseThrow(() ->
                                    new IllegalArgumentException(
                                            "Invoice not found"
                                    )
                            );

            validateSupplierConsistency(
                    request,
                    invoice,
                    payment
            );

            validateAllocationState(
                    invoice,
                    payment
            );

            validationService.validateAllocationAmount(
                    invoice.getOutstandingAmount(),
                    payment.getUnappliedAmount(),
                    request.getAllocationAmount()
            );

            SupplierPaymentAllocation allocation =
                    createAllocation(
                            request,
                            invoice,
                            payment,
                            tenantId
                    );

            invoiceService.applyAllocation(
                    invoice,
                    request.getAllocationAmount()
            );

            paymentService.applyAllocation(
                    payment,
                    request.getAllocationAmount()
            );

            allocationRepository.save(
                    allocation
            );

            return mapResponse(
                    allocation,
                    invoice,
                    payment
            );

        } catch (
                ObjectOptimisticLockingFailureException ex
        ) {
            throw new IllegalStateException(
                    "Concurrent allocation detected. Retry operation."
            );
        }
    }

    private void validateSupplierConsistency(
            AllocateSupplierPaymentRequest request,
            PurchaseInvoice invoice,
            SupplierPayment payment
    ) {
        if (
                !invoice.getSupplier().getId()
                        .equals(request.getSupplierId())
        ) {
            throw new IllegalStateException(
                    "Invoice supplier mismatch"
            );
        }

        if (
                !payment.getSupplierId()
                        .equals(request.getSupplierId())
        ) {
            throw new IllegalStateException(
                    "Payment supplier mismatch"
            );
        }
    }

    private SupplierPaymentAllocation createAllocation(
            AllocateSupplierPaymentRequest request,
            PurchaseInvoice invoice,
            SupplierPayment payment,
            UUID tenantId
    ) {
        return SupplierPaymentAllocation.builder()
                .tenantId(tenantId)
                .branchId(request.getBranchId())
                .purchaseInvoice(invoice)
                .supplierPayment(payment)
                .allocationType(
                        ApAllocationType.PAYMENT_TO_INVOICE
                )
                .status(
                        PaymentAllocationStatus.ACTIVE
                )
                .allocatedAmount(
                        request.getAllocationAmount()
                )
                .allocatedAt(
                        LocalDateTime.now()
                )
                .allocatedBy(
                        currentUser()
                )
                .build();
    }

    private AllocationResponse mapResponse(
            SupplierPaymentAllocation allocation,
            PurchaseInvoice invoice,
            SupplierPayment payment
    ) {
        return AllocationResponse
                .builder()
                .allocationId(allocation.getId())
                .paymentId(payment.getId())
                .paymentNumber(payment.getDocumentNumber())
                .invoiceId(invoice.getId())
                .invoiceNumber(invoice.getDocumentNumber())
                .allocatedAmount(allocation.getAllocatedAmount())
                .remainingInvoiceBalance(invoice.getOutstandingAmount())
                .remainingPaymentBalance(payment.getUnappliedAmount())
                .allocatedAt(allocation.getAllocatedAt())
                .status(allocation.getStatus())
                .reversed(allocation.isReversed())
                .reversedAt(allocation.getReversedAt())
                .reversedBy(allocation.getReversedBy())
                .reversalReason(allocation.getReversalReason())
                .build();
    }

    @Transactional(readOnly = true)
    public AllocationPreviewResponse previewAutoAllocation(
            AutoAllocatePaymentRequest request
    ) {

        branchTenantGuard.validate(
                request.getBranchId()
        );

        List<PurchaseInvoice> invoices =
                invoiceRepository.findByTenantIdAndBranchIdAndSupplier_IdAndOutstandingAmountGreaterThanAndReversedFalseAndCancelledFalseOrderByDueDateAscDocumentDateAsc(
                                TenantContext.getTenantId(),
                                request.getBranchId(),
                                request.getSupplierId(),
                                BigDecimal.ZERO
                        );

        BigDecimal remaining =
                request.getAmount();

        List<AllocationPreviewItemDto> items =
                new ArrayList<>();

        for (PurchaseInvoice invoice : invoices) {

            if (remaining.compareTo(BigDecimal.ZERO) <= 0) {
                break;
            }

            BigDecimal allocatable =
                    invoice.getOutstandingAmount()
                            .min(remaining);

            remaining =
                    remaining.subtract(allocatable);

            items.add(
                    AllocationPreviewItemDto.builder()
                            .invoiceId(invoice.getId())
                            .invoiceNumber(invoice.getDocumentNumber())
                            .invoiceDate(invoice.getDocumentDate())
                            .dueDate(invoice.getDueDate())
                            .invoiceOutstanding(
                                    invoice.getOutstandingAmount()
                            )
                            .allocationAmount(
                                    allocatable
                            )
                            .remainingAfterAllocation(
                                    invoice.getOutstandingAmount()
                                            .subtract(allocatable)
                            )
                            .build()
            );
        }

        return AllocationPreviewResponse.builder()
                .requestedAmount(
                        request.getAmount()
                )
                .totalAllocated(
                        request.getAmount()
                                .subtract(remaining)
                )
                .remainingUnallocated(
                        remaining
                )
                .allocations(items)
                .build();
    }

    @Transactional
    public List<AllocationResponse> autoAllocate(
            AutoAllocatePaymentRequest request
    ) {
        branchTenantGuard.validate(
                request.getBranchId()
        );

        SupplierPayment payment =
                paymentRepository
                        .findLockedByTenantIdAndBranchIdAndId(
                                TenantContext.getTenantId(),
                                request.getBranchId(),
                                request.getPaymentId()
                        )
                        .orElseThrow(() ->
                                new IllegalArgumentException(
                                        "Supplier payment not found"
                                )
                        );

        if (
                !payment.getSupplierId()
                        .equals(request.getSupplierId())
        ) {
            throw new IllegalStateException(
                    "Supplier mismatch"
            );
        }

        if (!payment.isPosted()) {
            throw new IllegalStateException(
                    "Only posted payments may be auto-allocated"
            );
        }

        if (
                payment.getPostingStatus()
                        != FinancialPostingStatus.POSTED
        ) {
            throw new IllegalStateException(
                    "Payment is not fully posted"
            );
        }

        if (payment.isReversed()) {
            throw new IllegalStateException(
                    "Cannot allocate reversed payment"
            );
        }

        List<PurchaseInvoice> invoices =
                invoiceRepository.findByTenantIdAndBranchIdAndSupplier_IdAndOutstandingAmountGreaterThanAndReversedFalseAndCancelledFalseOrderByDueDateAscDocumentDateAsc(
                        TenantContext.getTenantId(),
                        request.getBranchId(),
                        request.getSupplierId(),
                        BigDecimal.ZERO
                );

        BigDecimal remaining =
                request.getAmount();

        if (request.getAmount().compareTo(payment.getUnappliedAmount()) > 0) {
            throw new IllegalStateException(
                    "Allocation amount exceeds unapplied payment balance"
            );
        }

        List<AllocationResponse> responses =
                new ArrayList<>();

        for (PurchaseInvoice invoice : invoices) {

            if (remaining.compareTo(BigDecimal.ZERO) <= 0) {
                break;
            }

            validateAllocationState(
                    invoice,
                    payment
            );

            BigDecimal allocationAmount =
                    invoice.getOutstandingAmount()
                            .min(remaining);

            remaining =
                    remaining.subtract(allocationAmount);

            responses.add(
                    allocate(
                            buildAutoRequest(
                                    request,
                                    invoice,
                                    allocationAmount
                            )
                    )
            );
        }

        return responses;
    }

    private void validateAllocationState(
            PurchaseInvoice invoice,
            SupplierPayment payment
    ) {
        if (!payment.isPosted()) {
            throw new IllegalStateException(
                    "Only posted payments may be allocated"
            );
        }

        if (
                payment.getPostingStatus()
                        != FinancialPostingStatus.POSTED
        ) {
            throw new IllegalStateException(
                    "Payment is not fully posted"
            );
        }

        if (
                invoice.getPostingStatus()
                        != FinancialPostingStatus.POSTED
        ) {
            throw new IllegalStateException(
                    "Invoice is not posted"
            );
        }

        if (payment.isReversed()) {
            throw new IllegalStateException(
                    "Cannot allocate reversed payment"
            );
        }

        if (invoice.isReversed()) {
            throw new IllegalStateException(
                    "Cannot allocate reversed invoice"
            );
        }

        if (invoice.isCancelled()) {
            throw new IllegalStateException(
                    "Cannot allocate cancelled invoice"
            );
        }
    }

    private AllocateSupplierPaymentRequest buildAutoRequest(
            AutoAllocatePaymentRequest request,
            PurchaseInvoice invoice,
            BigDecimal allocationAmount
    ) {

        AllocateSupplierPaymentRequest allocation =
                new AllocateSupplierPaymentRequest();

        allocation.setBranchId(
                request.getBranchId()
        );

        allocation.setSupplierId(
                request.getSupplierId()
        );

        allocation.setPaymentId(
                request.getPaymentId()
        );

        allocation.setPurchaseInvoiceId(
                invoice.getId()
        );

        allocation.setAllocationAmount(
                allocationAmount
        );

        return allocation;
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

    @Transactional
    public AllocationResponse reverse(
            UUID branchId,
            UUID allocationId,
            String reason
    ) {
        branchTenantGuard.validate(
                branchId
        );

        SupplierPaymentAllocation allocation =
                allocationRepository
                        .findLockedByTenantIdAndBranchIdAndId(
                                TenantContext.getTenantId(),
                                branchId,
                                allocationId
                        )
                        .orElseThrow(() ->
                                new IllegalArgumentException(
                                        "Allocation not found"
                                )
                        );

        if (allocation.isReversed()) {
            throw new IllegalStateException(
                    "Allocation already reversed"
            );
        }

        SupplierPayment payment =
                paymentRepository
                        .findLockedByTenantIdAndBranchIdAndId(
                                TenantContext.getTenantId(),
                                branchId,
                                allocation.getSupplierPayment().getId()
                        )
                        .orElseThrow();

        PurchaseInvoice invoice =
                invoiceRepository
                        .findLockedByTenantIdAndBranchIdAndId(
                                TenantContext.getTenantId(),
                                branchId,
                                allocation.getPurchaseInvoice().getId()
                        )
                        .orElseThrow();

        validateAllocationState(
                invoice,
                payment
        );

        BigDecimal amount =
                allocation.getAllocatedAmount();

        paymentService.reverseAllocation(
                payment,
                amount
        );

        invoiceService.reverseAllocation(
                invoice,
                amount
        );

        allocation.setReversed(true);

        allocation.setReversedAt(
                LocalDateTime.now()
        );

        allocation.setReversedBy(
                currentUser()
        );

        allocation.setReversalReason(
                reason
        );

        allocationRepository.save(
                allocation
        );

        paymentRepository.save(
                payment
        );

        invoiceRepository.save(
                invoice
        );

        return mapResponse(
                allocation,
                invoice,
                payment
        );
    }

    @Transactional
    public void reverseAllForPayment(
            UUID branchId,
            SupplierPayment payment,
            String reason
    ) {

        List<SupplierPaymentAllocation> allocations =
                allocationRepository
                        .findActiveLockedByTenantIdAndBranchIdAndPaymentId(
                                TenantContext.getTenantId(),
                                branchId,
                                payment.getId()
                        );

        for (
                SupplierPaymentAllocation allocation :
                allocations
        ) {

            PurchaseInvoice invoice =
                    invoiceRepository
                            .findLockedByTenantIdAndBranchIdAndId(
                                    TenantContext.getTenantId(),
                                    branchId,
                                    allocation.getPurchaseInvoice().getId()
                            )
                            .orElseThrow(() ->
                                    new IllegalStateException(
                                            "Invoice not found during allocation reversal"
                                    )
                            );

            /*
             * Reverse invoice/payment balances
             */
            BigDecimal amount =
                    allocation.getAllocatedAmount();

            paymentService.reverseAllocation(
                    payment,
                    amount
            );

            invoiceService.reverseAllocation(
                    invoice,
                    amount
            );

            /*
             * Preserve allocation audit trail
             */
            allocation.setReversed(true);

            allocation.setStatus(
                    PaymentAllocationStatus.REVERSED
            );

            allocation.setReversedAt(
                    LocalDateTime.now()
            );

            allocation.setReversedBy(
                    currentUser()
            );

            allocation.setReversalReason(
                    reason
            );

            allocationRepository.save(
                    allocation
            );

            invoiceRepository.save(
                    invoice
            );
        }

        paymentRepository.save(
                payment
        );
    }
}