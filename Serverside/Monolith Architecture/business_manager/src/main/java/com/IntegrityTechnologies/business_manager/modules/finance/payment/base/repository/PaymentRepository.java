package com.IntegrityTechnologies.business_manager.modules.finance.payment.base.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.model.Payment;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.enums.PaymentStatus;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface PaymentRepository
        extends JpaRepository<Payment, UUID> {

    Optional<Payment>
    findByTenantIdAndBranchIdAndId(
            UUID tenantId,
            UUID branchId,
            UUID id
    );

    List<Payment>
    findByTenantIdAndBranchIdAndSale_Id(
            UUID tenantId,
            UUID branchId,
            UUID saleId
    );

    Optional<Payment>
    findByTenantIdAndBranchIdAndProviderReference(
            UUID tenantId,
            UUID branchId,
            String providerReference
    );

    Optional<Payment>
    findByTenantIdAndBranchIdAndTransactionCode(
            UUID tenantId,
            UUID branchId,
            String transactionCode
    );

    Page<Payment>
    findByTenantIdAndBranchId(
            UUID tenantId,
            UUID branchId,
            Pageable pageable
    );

    Page<Payment>
    findByTenantIdAndBranchIdAndMethodIgnoreCase(
            UUID tenantId,
            UUID branchId,
            String method,
            Pageable pageable
    );

    Page<Payment>
    findByTenantIdAndBranchIdAndStatus(
            UUID tenantId,
            UUID branchId,
            PaymentStatus status,
            Pageable pageable
    );

    List<Payment>
    findByTenantIdAndBranchIdAndTimestampBetween(
            UUID tenantId,
            UUID branchId,
            LocalDateTime from,
            LocalDateTime to
    );
}