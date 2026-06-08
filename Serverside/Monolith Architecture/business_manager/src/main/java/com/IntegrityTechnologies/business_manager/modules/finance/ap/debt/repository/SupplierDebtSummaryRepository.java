package com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.projection.SupplierDebtSummaryProjection;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.domain.PurchaseInvoice;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.Repository;
import org.springframework.data.repository.query.Param;

import java.time.LocalDate;
import java.util.Optional;
import java.util.UUID;

public interface SupplierDebtSummaryRepository extends Repository<PurchaseInvoice, UUID> {

    @Query("""
            SELECT
                pi.supplier.id AS supplierId,
                pi.supplier.name AS supplierName,
                COALESCE(SUM(pi.outstandingAmount), 0) AS totalOutstanding,
                COALESCE(SUM(
                    CASE
                        WHEN pi.dueDate < :today
                        THEN pi.outstandingAmount
                        ELSE 0
                    END
                ), 0) AS overdueAmount,
                COALESCE((
                    SELECT SUM(sp.unappliedAmount)
                    FROM SupplierPayment sp
                    WHERE sp.tenantId = :tenantId
                    AND sp.branchId = :branchId
                    AND sp.supplierId = :supplierId
                    AND sp.reversed = false
                ), 0) AS unappliedPayments,
                (
                    COALESCE(SUM(pi.outstandingAmount), 0)
                    -
                    COALESCE((
                        SELECT SUM(sp.unappliedAmount)
                        FROM SupplierPayment sp
                        WHERE sp.tenantId = :tenantId
                        AND sp.branchId = :branchId
                        AND sp.supplierId = :supplierId
                        AND sp.reversed = false
                    ), 0)
                ) AS netPayable,
                COUNT(pi.id) AS openBills,
                SUM(
                    CASE
                        WHEN pi.dueDate < :today
                        THEN 1
                        ELSE 0
                    END
                ) AS overdueBills,
                MIN(pi.dueDate) AS oldestDueDate,
                (
                    SELECT MAX(sp.paymentDate)
                    FROM SupplierPayment sp
                    WHERE sp.tenantId = :tenantId
                    AND sp.branchId = :branchId
                    AND sp.supplierId = :supplierId
                    AND sp.reversed = false
                ) AS lastPaymentDate
            FROM PurchaseInvoice pi
            WHERE pi.tenantId = :tenantId
            AND pi.branchId = :branchId
            AND pi.supplier.id = :supplierId
            AND pi.reversed = false
            AND pi.cancelled = false
            AND pi.outstandingAmount > 0
            GROUP BY
                pi.supplier.id,
                pi.supplier.name
            """)
    Optional<SupplierDebtSummaryProjection> findSupplierDebtSummary(
            UUID tenantId,
            UUID branchId,
            UUID supplierId,
            LocalDate today
    );

    @Query("""
                SELECT
                    pi.supplier.id AS supplierId,
                    pi.supplier.name AS supplierName,

                    COALESCE(SUM(pi.outstandingAmount), 0) AS totalOutstanding,

                    COALESCE(SUM(
                        CASE
                            WHEN pi.dueDate < :today
                            THEN pi.outstandingAmount
                            ELSE 0
                        END
                    ), 0) AS overdueAmount,

                    COALESCE((
                        SELECT SUM(sp.unappliedAmount)
                        FROM SupplierPayment sp
                        WHERE sp.tenantId = :tenantId
                        AND sp.branchId = :branchId
                        AND sp.supplierId = pi.supplier.id
                        AND sp.reversed = false
                    ), 0) AS unappliedPayments,

                    (
                        COALESCE(SUM(pi.outstandingAmount), 0)
                        -
                        COALESCE((
                            SELECT SUM(sp.unappliedAmount)
                            FROM SupplierPayment sp
                            WHERE sp.tenantId = :tenantId
                            AND sp.branchId = :branchId
                            AND sp.supplierId = pi.supplier.id
                            AND sp.reversed = false
                        ), 0)
                    ) AS netPayable,

                    COUNT(pi.id) AS openBills,

                    SUM(
                        CASE
                            WHEN pi.dueDate < :today
                            THEN 1
                            ELSE 0
                        END
                    ) AS overdueBills,

                    MIN(pi.dueDate) AS oldestDueDate,

                    (
                        SELECT MAX(sp.paymentDate)
                        FROM SupplierPayment sp
                        WHERE sp.tenantId = :tenantId
                        AND sp.branchId = :branchId
                        AND sp.supplierId = pi.supplier.id
                        AND sp.reversed = false
                    ) AS lastPaymentDate

                FROM PurchaseInvoice pi

                WHERE pi.tenantId = :tenantId
                AND pi.branchId = :branchId
                AND pi.reversed = false
                AND pi.cancelled = false
                AND pi.outstandingAmount > 0

                AND (
                    :search IS NULL
                    OR LOWER(pi.supplier.name)
                    LIKE LOWER(CONCAT('%', :search, '%'))
                )

                GROUP BY
                    pi.supplier.id,
                    pi.supplier.name

                HAVING (
                    :hasOverdue IS NULL
                    OR (
                        :hasOverdue = true
                        AND SUM(
                            CASE
                                WHEN pi.dueDate < :today
                                THEN 1
                                ELSE 0
                            END
                        ) > 0
                    )
                    OR (
                        :hasOverdue = false
                        AND SUM(
                            CASE
                                WHEN pi.dueDate < :today
                                THEN 1
                                ELSE 0
                            END
                        ) = 0
                    )
                )

                AND (
                    :hasUnapplied IS NULL
                    OR (
                        :hasUnapplied = true
                        AND COALESCE((
                            SELECT SUM(sp.unappliedAmount)
                            FROM SupplierPayment sp
                            WHERE sp.tenantId = :tenantId
                            AND sp.branchId = :branchId
                            AND sp.supplierId = pi.supplier.id
                            AND sp.reversed = false
                        ), 0) > 0
                    )
                    OR (
                        :hasUnapplied = false
                        AND COALESCE((
                            SELECT SUM(sp.unappliedAmount)
                            FROM SupplierPayment sp
                            WHERE sp.tenantId = :tenantId
                            AND sp.branchId = :branchId
                            AND sp.supplierId = pi.supplier.id
                            AND sp.reversed = false
                        ), 0) = 0
                    )
                )
            """)
    Page<SupplierDebtSummaryProjection> findDebtSummary(
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId,
            @Param("today") LocalDate today,
            @Param("search") String search,
            @Param("hasOverdue") Boolean hasOverdue,
            @Param("hasUnapplied") Boolean hasUnapplied,
            Pageable pageable
    );
}