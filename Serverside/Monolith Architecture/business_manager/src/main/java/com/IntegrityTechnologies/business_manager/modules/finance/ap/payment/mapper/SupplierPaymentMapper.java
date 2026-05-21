package com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.mapper;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.dto.SupplierPaymentResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.model.SupplierPayment;
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
                .fundingAccountId(
                        payment.getFundingAccountId()
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
                .paymentDate(
                        payment.getPaymentDate()
                )
                .postedAt(payment.getPostedAt())
                .postedBy(payment.getPostedBy())
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