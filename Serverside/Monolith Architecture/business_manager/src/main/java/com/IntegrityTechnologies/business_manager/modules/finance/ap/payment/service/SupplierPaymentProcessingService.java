package com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.service;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.dto.AutoAllocatePaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.service.ApAllocationService;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.dto.CreateSupplierPaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.dto.ProcessSupplierPaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.dto.SupplierPaymentResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;

@Service
@RequiredArgsConstructor
public class SupplierPaymentProcessingService {

    private final SupplierPaymentService paymentService;
    private final SupplierPaymentPostingService postingService;
    private final ApAllocationService allocationService;

    @Transactional
    public SupplierPaymentResponse process(
            ProcessSupplierPaymentRequest request
    ) {

        SupplierPaymentResponse created =
                paymentService.create(
                        buildCreateRequest(request)
                );

        SupplierPaymentResponse current =
                created;

        if (request.isAutoPost()) {

            current =
                    postingService.post(
                            request.getBranchId(),
                            created.getId()
                    );

            if (
                    request.isAutoAllocate()
                            &&
                            current.getUnappliedAmount()
                                    .compareTo(BigDecimal.ZERO) > 0
            ) {

                AutoAllocatePaymentRequest auto =
                        new AutoAllocatePaymentRequest();

                auto.setBranchId(
                        request.getBranchId()
                );

                auto.setSupplierId(
                        request.getSupplierId()
                );

                auto.setPaymentId(
                        current.getId()
                );

                auto.setAmount(
                        current.getUnappliedAmount()
                );

                auto.setTargetInvoiceIds(
                        request.getTargetInvoiceIds()
                );

                allocationService.autoAllocate(
                        auto
                );

                current =
                        paymentService.details(
                                request.getBranchId(),
                                current.getId()
                        ).getPayment();
            }
        }

        return current;
    }

    private CreateSupplierPaymentRequest buildCreateRequest(
            ProcessSupplierPaymentRequest request
    ) {

        CreateSupplierPaymentRequest create =
                new CreateSupplierPaymentRequest();

        create.setBranchId(
                request.getBranchId()
        );

        create.setSupplierId(
                request.getSupplierId()
        );

        create.setFundingAccountId(
                request.getFundingAccountId()
        );

        create.setAmount(
                request.getAmount()
        );

        create.setMethod(
                request.getMethod()
        );

        create.setReference(
                request.getReference()
        );

        create.setPaymentDate(
                request.getPaymentDate()
        );

        return create;
    }
}