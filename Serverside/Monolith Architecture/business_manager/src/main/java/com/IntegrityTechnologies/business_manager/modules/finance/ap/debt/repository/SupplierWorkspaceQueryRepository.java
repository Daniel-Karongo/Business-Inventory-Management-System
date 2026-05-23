package com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.projection.InvoiceSettlementProjection;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.projection.PaymentSettlementProjection;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.projection.SupplierBillLineProjection;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.projection.SupplierBillProjection;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.projection.SupplierPaymentProjection;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.projection.SupplierTimelineProjection;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.domain.PurchaseInvoice;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.Repository;

import java.util.List;
import java.util.UUID;

@org.springframework.stereotype.Repository
public interface SupplierWorkspaceQueryRepository
        extends Repository<PurchaseInvoice, UUID> {

    @Query("""
                SELECT
                    pi.id AS invoiceId,
                    pi.documentNumber AS billNumber,
                    pi.documentDate AS invoiceDate,
                    pi.dueDate AS dueDate,
                    pi.totalAmount AS totalAmount,
                    pi.allocatedAmount AS paidAmount,
                    pi.outstandingAmount AS remainingAmount,
                    CAST(pi.status AS string) AS status
                FROM PurchaseInvoice pi
                WHERE pi.tenantId = :tenantId
                AND pi.branchId = :branchId
                AND pi.supplier.id = :supplierId
                AND pi.cancelled = false
                AND pi.reversed = false
                ORDER BY pi.dueDate ASC
            """)
    List<SupplierBillProjection> findBills(
            UUID tenantId,
            UUID branchId,
            UUID supplierId
    );

    @Query("""
                SELECT
                    pil.purchaseInvoice.id AS invoiceId,
                    pil.productNameSnapshot AS productName,
                    pil.variantNameSnapshot AS variantName,
                    pil.quantity AS quantity,
                    pil.unitCost AS unitCost,
                    pil.lineTotal AS totalCost
                FROM PurchaseInvoiceLine pil
                WHERE pil.tenantId = :tenantId
                AND pil.branchId = :branchId
                AND pil.purchaseInvoice.supplier.id = :supplierId
            """)
    List<SupplierBillLineProjection> findBillLines(
            UUID tenantId,
            UUID branchId,
            UUID supplierId
    );

    @Query("""
                SELECT
                    spa.purchaseInvoice.id AS invoiceId,
                    sp.id AS paymentId,
                    sp.documentNumber AS paymentNumber,
                    sp.paymentDate AS paymentDate,
                    spa.allocatedAmount AS allocatedAmount,
                    sp.method AS paymentMethod
                FROM SupplierPaymentAllocation spa
                JOIN spa.supplierPayment sp
                WHERE spa.tenantId = :tenantId
                AND spa.branchId = :branchId
                AND spa.status = 'ACTIVE'
                AND spa.purchaseInvoice.supplier.id = :supplierId
            """)
    List<InvoiceSettlementProjection> findInvoiceSettlements(
            UUID tenantId,
            UUID branchId,
            UUID supplierId
    );

    @Query("""
                SELECT
                    sp.id AS paymentId,
                    sp.documentNumber AS paymentNumber,
                    sp.paymentDate AS paymentDate,
                    sp.amount AS amount,
                    sp.allocatedAmount AS allocatedAmount,
                    sp.unappliedAmount AS unappliedAmount,
                    sp.status AS status,
                    sp.method AS paymentMethod,
                    sp.reference AS reference,
                    sp.posted AS posted,
                    sp.reversed AS reversed,
                    sp.postingStatus AS postingStatus
                FROM SupplierPayment sp
                WHERE sp.tenantId = :tenantId
                AND sp.branchId = :branchId
                AND sp.supplierId = :supplierId
                ORDER BY sp.paymentDate DESC
            """)
    List<SupplierPaymentProjection> findPayments(
            UUID tenantId,
            UUID branchId,
            UUID supplierId
    );

    @Query("""
                SELECT
                    spa.id AS allocationId,
                    sp.id AS paymentId,
                    pi.id AS invoiceId,
                    pi.documentNumber AS billNumber,
                    pi.documentDate AS invoiceDate,
                    spa.allocatedAmount AS allocatedAmount,
                    spa.status AS status,
                    spa.reversed AS reversed,
                    spa.reversedAt AS reversedAt,
                    spa.reversedBy AS reversedBy,
                    spa.reversalReason AS reversalReason
                FROM SupplierPaymentAllocation spa
                JOIN spa.supplierPayment sp
                JOIN spa.purchaseInvoice pi
                WHERE spa.tenantId = :tenantId
                AND spa.branchId = :branchId
                AND sp.supplierId = :supplierId
            """)
    List<PaymentSettlementProjection> findPaymentSettlements(
            UUID tenantId,
            UUID branchId,
            UUID supplierId
    );

    @Query(value = """
                SELECT
                    pi.document_date AS timestamp,
                    'Bill Created' AS activity,
                    pi.document_number AS reference,
                    pi.total_amount AS debitAmount,
                    0 AS creditAmount
                FROM purchase_invoices pi
                WHERE pi.tenant_id = :tenantId
                AND pi.branch_id = :branchId
                AND pi.supplier_id = :supplierId

                UNION ALL

                SELECT
                    sp.payment_date AS timestamp,
                    'Supplier Payment' AS activity,
                    sp.document_number AS reference,
                    0 AS debitAmount,
                    sp.amount AS creditAmount
                FROM supplier_payments sp
                WHERE sp.tenant_id = :tenantId
                AND sp.branch_id = :branchId
                AND sp.supplier_id = :supplierId
                AND sp.reversed = false
            """, nativeQuery = true)
    List<SupplierTimelineProjection> findTimeline(
            UUID tenantId,
            UUID branchId,
            UUID supplierId
    );
}