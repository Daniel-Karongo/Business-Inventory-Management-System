package com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.service;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.domain.SupplierPaymentAllocation;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.dto.AllocateSupplierPaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.enums.ApAllocationType;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.enums.PaymentAllocationStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.repository.SupplierPaymentAllocationRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.domain.PurchaseInvoice;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.enums.PurchaseInvoiceStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.service.PurchaseInvoiceService;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.shared.validation.ApValidationService;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.domain.SupplierPayment;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.repository.SupplierPaymentRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class ApAllocationService {

    private final SupplierPaymentAllocationRepository
            allocationRepository;

    private final SupplierPaymentRepository
            paymentRepository;

    private final PurchaseInvoiceService
            invoiceService;

    private final ApValidationService
            validationService;

    private final BranchTenantGuard
            branchTenantGuard;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    @Transactional
    public SupplierPaymentAllocation allocate(
            AllocateSupplierPaymentRequest request
    ) {

        try {

            branchTenantGuard.validate(
                    request.getBranchId()
            );

            PurchaseInvoice invoice =
                    invoiceService.getManaged(
                            request.getBranchId(),
                            request.getPurchaseInvoiceId()
                    );

            SupplierPayment payment =
                    paymentRepository
                            .findByTenantIdAndBranchIdAndId(
                                    tenantId(),
                                    request.getBranchId(),
                                    request.getPaymentId()
                            )
                            .orElseThrow(() ->
                                    new IllegalArgumentException(
                                            "Supplier payment not found"
                                    )
                            );

            if (
                    !invoice.getSupplier().getId()
                            .equals(payment.getSupplierId())
            ) {
                throw new IllegalStateException(
                        "Payment supplier does not match invoice supplier"
                );
            }

            validationService.validateAllocationAmount(
                    invoice.getOutstandingAmount(),
                    payment.getUnappliedAmount(),
                    request.getAllocationAmount()
            );

            BigDecimal allocationAmount =
                    request.getAllocationAmount();

            SupplierPaymentAllocation allocation =
                    SupplierPaymentAllocation.builder()
                            .tenantId(tenantId())
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
                                    allocationAmount
                            )
                            .allocatedAt(
                                    LocalDateTime.now()
                            )
                            .allocatedBy(
                                    currentUser()
                            )
                            .build();

            /*
             * INVOICE
             */

            BigDecimal newInvoiceAllocated =
                    invoice.getAllocatedAmount()
                            .add(allocationAmount);

            BigDecimal newOutstanding =
                    invoice.getOutstandingAmount()
                            .subtract(allocationAmount);

            if (
                    newOutstanding.compareTo(BigDecimal.ZERO) < 0
            ) {
                throw new IllegalStateException(
                        "Invoice outstanding amount cannot become negative"
                );
            }

            invoice.setAllocatedAmount(
                    newInvoiceAllocated
            );

            invoice.setOutstandingAmount(
                    newOutstanding
            );

            if (
                    newOutstanding.compareTo(BigDecimal.ZERO) == 0
            ) {

                invoice.setFullyAllocated(true);

                invoice.setStatus(
                        PurchaseInvoiceStatus.PAID
                );

            } else {

                invoice.setStatus(
                        PurchaseInvoiceStatus.PARTIALLY_PAID
                );
            }

            /*
             * PAYMENT
             */

            BigDecimal newPaymentAllocated =
                    payment.getAllocatedAmount()
                            .add(allocationAmount);

            BigDecimal newUnapplied =
                    payment.getUnappliedAmount()
                            .subtract(allocationAmount);

            if (
                    newUnapplied.compareTo(BigDecimal.ZERO) < 0
            ) {
                throw new IllegalStateException(
                        "Payment unapplied amount cannot become negative"
                );
            }

            payment.setAllocatedAmount(
                    newPaymentAllocated
            );

            payment.setUnappliedAmount(
                    newUnapplied
            );

            if (
                    newUnapplied.compareTo(BigDecimal.ZERO) == 0
            ) {
                payment.setFullyAllocated(true);
            }

            /*
             * INVARIANT VERIFICATION
             */

            if (
                    invoice.getAllocatedAmount()
                            .add(invoice.getOutstandingAmount())
                            .compareTo(invoice.getTotalAmount()) != 0
            ) {
                throw new IllegalStateException(
                        "Invoice allocation invariant violation"
                );
            }

            if (
                    payment.getAllocatedAmount()
                            .add(payment.getUnappliedAmount())
                            .compareTo(payment.getAmount()) != 0
            ) {
                throw new IllegalStateException(
                        "Payment allocation invariant violation"
                );
            }

            return allocationRepository.save(
                    allocation
            );

        } catch (
                ObjectOptimisticLockingFailureException ex
        ) {

            throw new IllegalStateException(
                    "Concurrent allocation detected. Please retry."
            );
        }
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