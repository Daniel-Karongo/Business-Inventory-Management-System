package com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.domain.SupplierPaymentAllocation;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.UUID;

public interface SupplierPaymentAllocationRepository
        extends JpaRepository<SupplierPaymentAllocation, UUID> {

    List<SupplierPaymentAllocation>
    findByTenantIdAndBranchIdAndPurchaseInvoice_Id(
            UUID tenantId,
            UUID branchId,
            UUID invoiceId
    );

    List<SupplierPaymentAllocation>
    findByTenantIdAndBranchIdAndSupplierPayment_Id(
            UUID tenantId,
            UUID branchId,
            UUID paymentId
    );
}