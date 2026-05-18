package com.IntegrityTechnologies.business_manager.modules.finance.payment.base.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.domain.SupplierPayment;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface SupplierPaymentRepository
        extends JpaRepository<SupplierPayment, UUID> {

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
}