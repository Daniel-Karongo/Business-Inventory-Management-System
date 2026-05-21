package com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.domain.PurchaseInvoice;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.enums.PurchaseInvoiceStatus;
import jakarta.persistence.LockModeType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface PurchaseInvoiceRepository
        extends JpaRepository<PurchaseInvoice, UUID> {

    @EntityGraph(attributePaths = {
            "supplier",
            "lines"
    })
    List<PurchaseInvoice>
    findDetailedByTenantIdAndBranchIdAndSupplier_Id(
            UUID tenantId,
            UUID branchId,
            UUID supplierId
    );

    @EntityGraph(attributePaths = {
            "supplier"
    })
    List<PurchaseInvoice> findByTenantIdAndBranchIdAndSupplier_IdAndOutstandingAmountGreaterThanAndReversedFalseAndCancelledFalseOrderByDueDateAscDocumentDateAsc(
            UUID tenantId,
            UUID branchId,
            UUID supplierId,
            BigDecimal outstandingAmount
    );

    Optional<PurchaseInvoice>
    findByTenantIdAndBranchIdAndId(
            UUID tenantId,
            UUID branchId,
            UUID id
    );

    Optional<PurchaseInvoice>
    findByTenantIdAndBranchIdAndDocumentNumber(
            UUID tenantId,
            UUID branchId,
            String documentNumber
    );

    Optional<PurchaseInvoice>
    findByTenantIdAndBranchIdAndSupplierInvoiceNumber(
            UUID tenantId,
            UUID branchId,
            String supplierInvoiceNumber
    );

    Page<PurchaseInvoice>
    findByTenantIdAndBranchIdOrderByDocumentDateDesc(
            UUID tenantId,
            UUID branchId,
            Pageable pageable
    );

    Page<PurchaseInvoice>
    findByTenantIdAndBranchIdAndStatusOrderByDocumentDateDesc(
            UUID tenantId,
            UUID branchId,
            PurchaseInvoiceStatus status,
            Pageable pageable
    );

    Page<PurchaseInvoice>
    findByTenantIdAndBranchIdAndDueDateBeforeAndOutstandingAmountGreaterThanOrderByDueDateAsc(
            UUID tenantId,
            UUID branchId,
            LocalDate dueDate,
            java.math.BigDecimal minimumOutstanding,
            Pageable pageable
    );

    List<PurchaseInvoice> findByTenantIdAndBranchId(
            UUID tenantId,
            UUID branchId
    );

    List<PurchaseInvoice> findByTenantIdAndBranchIdAndSupplier_Id(
            UUID tenantId,
            UUID branchId,
            UUID supplierId
    );

    List<PurchaseInvoice> findByTenantId(
            UUID tenantId
    );

    List<PurchaseInvoice> findByTenantIdAndSupplier_Id(
            UUID tenantId,
            UUID supplierId
    );

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("""
             SELECT pi
             FROM PurchaseInvoice pi
             WHERE pi.tenantId = :tenantId
             AND pi.branchId = :branchId
             AND pi.id = :invoiceId
            """)
    Optional<PurchaseInvoice>
    findLockedByTenantIdAndBranchIdAndId(
            UUID tenantId,
            UUID branchId,
            UUID invoiceId
    );
}