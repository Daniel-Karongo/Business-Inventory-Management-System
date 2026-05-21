package com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.domain.SupplierPaymentAllocation;
import jakarta.persistence.LockModeType;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface SupplierPaymentAllocationRepository
        extends JpaRepository<SupplierPaymentAllocation, UUID> {

    @EntityGraph(attributePaths = {
            "purchaseInvoice",
            "supplierPayment"
    })
    List<SupplierPaymentAllocation>
    findByTenantIdAndBranchIdAndPurchaseInvoice_Id(
            UUID tenantId,
            UUID branchId,
            UUID invoiceId
    );

    @EntityGraph(attributePaths = {
            "purchaseInvoice",
            "supplierPayment"
    })
    List<SupplierPaymentAllocation>
    findByTenantIdAndBranchIdAndSupplierPayment_Id(
            UUID tenantId,
            UUID branchId,
            UUID paymentId
    );

    @EntityGraph(attributePaths = {
            "purchaseInvoice",
            "supplierPayment"
    })
    List<SupplierPaymentAllocation>
    findByTenantIdAndBranchIdAndPurchaseInvoice_Supplier_Id(
            UUID tenantId,
            UUID branchId,
            UUID supplierId
    );

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("""
             SELECT spa
             FROM SupplierPaymentAllocation spa
             WHERE spa.tenantId = :tenantId
             AND spa.branchId = :branchId
             AND spa.id = :allocationId
            """)
    Optional<SupplierPaymentAllocation>
    findLockedByTenantIdAndBranchIdAndId(
            UUID tenantId,
            UUID branchId,
            UUID allocationId
    );

    boolean existsByTenantIdAndBranchIdAndSupplierPayment_IdAndReversedFalse(
            UUID tenantId,
            UUID branchId,
            UUID paymentId
    );
}