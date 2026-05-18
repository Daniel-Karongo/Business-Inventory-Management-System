package com.IntegrityTechnologies.business_manager.modules.finance.payment.base.mapper;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.domain.SupplierPayment;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.dto.SupplierPaymentResponse;
import org.springframework.stereotype.Component;

@Component
public class SupplierPaymentMapper {

    public SupplierPaymentResponse toResponse(
            SupplierPayment payment
    ) {

        return SupplierPaymentResponse
                .builder()
                .id(payment.getId())
                .documentNumber(
                        payment.getDocumentNumber()
                )
                .supplierId(
                        payment.getSupplierId()
                )
                .amount(payment.getAmount())
                .allocatedAmount(
                        payment.getAllocatedAmount()
                )
                .unappliedAmount(
                        payment.getUnappliedAmount()
                )
                .fullyAllocated(
                        payment.isFullyAllocated()
                )
                .status(payment.getStatus())
                .method(payment.getMethod())
                .reference(payment.getReference())
                .posted(payment.isPosted())
                .journalEntryId(
                        payment.getJournalEntryId()
                )
                .paymentDate(
                        payment.getPaymentDate()
                )
                .paidAt(payment.getPaidAt())
                .paidBy(payment.getPaidBy())
                .reversed(payment.isReversed())
                .reversedAt(
                        payment.getReversedAt()
                )
                .reversedBy(
                        payment.getReversedBy()
                )
                .reversalReason(
                        payment.getReversalReason()
                )
                .build();
    }
}