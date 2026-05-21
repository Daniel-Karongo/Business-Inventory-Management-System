package com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.enums.SupplierPaymentStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.model.SupplierPayment;
import jakarta.persistence.LockModeType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface SupplierPaymentRepository
        extends JpaRepository<SupplierPayment, UUID>,
        JpaSpecificationExecutor<SupplierPayment> {

    List<SupplierPayment>
    findByTenantIdAndBranchIdAndSupplierIdAndReversedFalseOrderByPaymentDateDesc(
            UUID tenantId,
            UUID branchId,
            UUID supplierId
    );

    Page<SupplierPayment>
    findByTenantIdAndBranchIdOrderByPaidAtDesc(
            UUID tenantId,
            UUID branchId,
            Pageable pageable
    );

    Optional<SupplierPayment> findByTenantIdAndBranchIdAndId(UUID tenantId, UUID branchId, UUID paymentId);

    List<SupplierPayment> findByTenantIdAndBranchIdAndSupplierId(
            UUID tenantId,
            UUID branchId,
            UUID supplierId
    );

    List<SupplierPayment> findByTenantIdAndSupplierId(
            UUID tenantId,
            UUID supplierId
    );

    Page<SupplierPayment>
    findByTenantIdAndBranchIdAndSupplierIdOrderByPaymentDateDesc(
            UUID tenantId,
            UUID branchId,
            UUID supplierId,
            Pageable pageable
    );

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("""
             SELECT sp
             FROM SupplierPayment sp
             WHERE sp.tenantId = :tenantId
             AND sp.branchId = :branchId
             AND sp.id = :paymentId
            """)
    Optional<SupplierPayment> findLockedByTenantIdAndBranchIdAndId(
            UUID tenantId,
            UUID branchId,
            UUID paymentId
    );

    Page<SupplierPayment>
    findByTenantIdAndBranchIdAndStatusOrderByPaymentDateDesc(
            UUID tenantId,
            UUID branchId,
            SupplierPaymentStatus status,
            Pageable pageable
    );

    Page<SupplierPayment>
    findByTenantIdAndBranchIdAndSupplierIdAndStatusOrderByPaymentDateDesc(
            UUID tenantId,
            UUID branchId,
            UUID supplierId,
            SupplierPaymentStatus status,
            Pageable pageable
    );
}