package com.IntegrityTechnologies.business_manager.modules.finance.payment.base.service;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.enums.SupplierPaymentStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.domain.SupplierPayment;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.dto.CreateSupplierPaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.dto.SupplierPaymentResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.mapper.SupplierPaymentMapper;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.repository.SupplierPaymentRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.shared.service.DocumentNumberGeneratorService;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class SupplierPaymentService {

    private final SupplierPaymentRepository
            repository;

    private final BranchTenantGuard
            branchTenantGuard;

    private final SupplierPaymentMapper
            mapper;

    private final DocumentNumberGeneratorService
            numberGeneratorService;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    @Transactional
    public SupplierPaymentResponse create(
            CreateSupplierPaymentRequest request
    ) {

        branchTenantGuard.validate(
                request.getBranchId()
        );

        if (
                request.getAmount() == null
                        || request.getAmount()
                        .compareTo(BigDecimal.ZERO) <= 0
        ) {
            throw new IllegalArgumentException(
                    "Amount must be greater than zero"
            );
        }

        SupplierPayment payment =
                SupplierPayment.builder()
                        .tenantId(tenantId())
                        .branchId(request.getBranchId())
                        .documentNumber(
                                numberGeneratorService
                                        .nextSupplierPaymentNumber(
                                                request.getBranchId()
                                        )
                        )
                        .supplierId(
                                request.getSupplierId()
                        )
                        .amount(
                                request.getAmount()
                        )
                        .allocatedAmount(
                                BigDecimal.ZERO
                        )
                        .unappliedAmount(
                                request.getAmount()
                        )
                        .fullyAllocated(false)
                        .status(
                                SupplierPaymentStatus.POSTED
                        )
                        .method(
                                request.getMethod()
                                        .toUpperCase()
                        )
                        .reference(
                                request.getReference()
                        )
                        .posted(true)
                        .postedAt(
                                LocalDateTime.now()
                        )
                        .postedBy(
                                currentUser()
                        )
                        .paymentDate(
                                request.getPaymentDate()
                        )
                        .paidAt(
                                LocalDateTime.now()
                        )
                        .paidBy(
                                currentUser()
                        )
                        .build();

        return mapper.toResponse(
                repository.save(payment)
        );
    }

    public Page<SupplierPaymentResponse> list(
            UUID branchId,
            Pageable pageable
    ) {

        branchTenantGuard.validate(
                branchId
        );

        return repository
                .findByTenantIdAndBranchIdOrderByPaidAtDesc(
                        tenantId(),
                        branchId,
                        pageable
                )
                .map(mapper::toResponse);
    }

    public SupplierPayment getManaged(
            UUID branchId,
            UUID paymentId
    ) {

        branchTenantGuard.validate(
                branchId
        );

        return repository
                .findByTenantIdAndBranchIdAndId(
                        tenantId(),
                        branchId,
                        paymentId
                )
                .orElseThrow(() ->
                        new IllegalArgumentException(
                                "Supplier payment not found"
                        )
                );
    }

    @Transactional
    public SupplierPaymentResponse reverse(
            UUID branchId,
            UUID paymentId,
            String reason
    ) {

        SupplierPayment payment =
                getManaged(
                        branchId,
                        paymentId
                );

        if (payment.isReversed()) {
            throw new IllegalStateException(
                    "Payment already reversed"
            );
        }

        /*
         * DO NOT ALLOW REVERSAL OF
         * ALLOCATED PAYMENTS
         */

        if (
                payment.getAllocatedAmount()
                        .compareTo(BigDecimal.ZERO) > 0
        ) {
            throw new IllegalStateException(
                    "Cannot reverse allocated payment"
            );
        }

        payment.setReversed(true);

        payment.setReversedAt(
                LocalDateTime.now()
        );

        payment.setReversedBy(
                currentUser()
        );

        payment.setReversalReason(
                reason
        );

        payment.setStatus(
                SupplierPaymentStatus.REVERSED
        );

        return mapper.toResponse(
                repository.save(payment)
        );
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