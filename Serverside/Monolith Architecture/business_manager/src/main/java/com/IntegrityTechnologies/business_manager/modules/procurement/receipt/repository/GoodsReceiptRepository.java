package com.IntegrityTechnologies.business_manager.modules.procurement.receipt.repository;

import com.IntegrityTechnologies.business_manager.modules.procurement.receipt.domain.GoodsReceipt;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface GoodsReceiptRepository extends JpaRepository<GoodsReceipt, UUID> {

    List<GoodsReceipt>
    findByTenantIdAndBranchIdAndSupplierIdAndInvoicedFalse(
            UUID tenantId,
            UUID branchId,
            UUID supplierId
    );

    Optional<GoodsReceipt> findByTenantIdAndBranchIdAndId(UUID tenantId, UUID branchId, UUID receiptId);

    List<GoodsReceipt> findByTenantIdAndBranchIdAndMatchedInvoiceId(
            UUID tenantId,
            UUID branchId,
            UUID matchedInvoiceId
    );

    Optional<GoodsReceipt>
    findByTenantIdAndBranchIdAndReceiptNumber(
            UUID tenantId,
            UUID branchId,
            String receiptNumber
    );
}