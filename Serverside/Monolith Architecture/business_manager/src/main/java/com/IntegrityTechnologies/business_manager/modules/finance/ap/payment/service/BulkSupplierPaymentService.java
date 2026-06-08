package com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.service;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.projection.SupplierDebtSummaryProjection;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.dto.BulkSupplierPaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.dto.BulkSupplierPaymentResult;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.dto.ProcessSupplierPaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.repository.SupplierDebtSummaryRepository;
import com.IntegrityTechnologies.business_manager.modules.person.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.person.supplier.repository.SupplierRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class BulkSupplierPaymentService {

    private final SupplierDebtSummaryRepository debtRepository;
    private final SupplierRepository supplierRepository;
    private final SupplierPaymentProcessingService processingService;

    @Transactional(
            rollbackFor = Exception.class
    )
    public List<BulkSupplierPaymentResult> process(
            BulkSupplierPaymentRequest request
    ) {
        UUID tenantId =
                TenantContext.getTenantId();

        List<PendingSupplierPayment> pending =
                new ArrayList<>();

        BigDecimal totalRequired =
                BigDecimal.ZERO;

        for (UUID supplierId : request.getSupplierIds()) {

            Supplier supplier =
                    supplierRepository
                            .findActiveById(
                                    supplierId,
                                    tenantId,
                                    request.getBranchId()
                            )
                            .orElseThrow(
                                    () ->
                                            new IllegalArgumentException(
                                                    "Supplier not found: "
                                                            + supplierId
                                            )
                            );

            SupplierDebtSummaryProjection debt =
                    debtRepository
                            .findSupplierDebtSummary(
                                    tenantId,
                                    request.getBranchId(),
                                    supplierId,
                                    LocalDate.now()
                            )
                            .orElseThrow(
                                    () ->
                                            new IllegalStateException(
                                                    "Supplier has no payable balance: "
                                                            + supplier.getName()
                                            )
                            );

            BigDecimal payable =
                    Optional.ofNullable(
                                    debt.getNetPayable()
                            )
                            .orElse(BigDecimal.ZERO)
                            .max(BigDecimal.ZERO);

            if (
                    payable.compareTo(
                            BigDecimal.ZERO
                    ) <= 0
            ) {
                throw new IllegalStateException(
                        "Supplier "
                                + supplier.getName()
                                + " has no payable balance"
                );
            }

            pending.add(
                    new PendingSupplierPayment(
                            supplierId,
                            supplier.getName(),
                            payable
                    )
            );

            totalRequired =
                    totalRequired.add(
                            payable
                    );
        }

        List<BulkSupplierPaymentResult> results =
                new ArrayList<>();

        for (PendingSupplierPayment payment : pending) {

            ProcessSupplierPaymentRequest requestDto =
                    new ProcessSupplierPaymentRequest();

            requestDto.setBranchId(
                    request.getBranchId()
            );

            requestDto.setSupplierId(
                    payment.supplierId()
            );

            requestDto.setFundingAccountId(
                    request.getFundingAccountId()
            );

            requestDto.setMethod(
                    request.getMethod()
            );

            requestDto.setAmount(
                    payment.amount()
            );

            requestDto.setReference(
                    request.getReference()
            );

            requestDto.setPaymentDate(
                    request.getPaymentDate()
            );

            requestDto.setAutoPost(
                    true
            );

            requestDto.setAutoAllocate(
                    request.isAutoAllocate()
            );

            processingService.process(
                    requestDto
            );

            results.add(
                    BulkSupplierPaymentResult
                            .builder()
                            .supplierId(
                                    payment.supplierId()
                            )
                            .supplierName(
                                    payment.supplierName()
                            )
                            .amountPaid(
                                    payment.amount()
                            )
                            .status(
                                    "SUCCESS"
                            )
                            .build()
            );
        }

        return results;
    }

    private record PendingSupplierPayment(
            UUID supplierId,
            String supplierName,
            BigDecimal amount
    ) {
    }
}