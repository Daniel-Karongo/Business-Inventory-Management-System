package com.IntegrityTechnologies.business_manager.modules.finance.sales.base.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.model.Sale;
import jakarta.persistence.LockModeType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Repository
public interface SaleRepository extends JpaRepository<Sale, UUID> {
    List<Sale> findByCustomerId(UUID customerId);

    @Lock(LockModeType.OPTIMISTIC_FORCE_INCREMENT)
    @Query("select s from Sale s where s.id = :id")
    Sale lockForUpdate(@Param("id") UUID id);

    @Query("""
        SELECT DATE(s.createdAt), SUM(li.lineTotal)
        FROM Sale s
        JOIN s.lineItems li
        WHERE li.branchId = :branchId
          AND s.status = 'COMPLETED'
        GROUP BY DATE(s.createdAt)
        ORDER BY DATE(s.createdAt)
        """)
    List<Object[]> revenueTrendByDay(@Param("branchId") UUID branchId);

    @Query("""
    SELECT DISTINCT s
    FROM Sale s
    JOIN s.lineItems li
    WHERE (:status IS NULL OR s.status = :status)
      AND (:customerId IS NULL OR s.customerId = :customerId)
      AND (:branchId IS NULL OR li.branchId = :branchId)
      AND (:from IS NULL OR s.createdAt >= :from)
      AND (:to IS NULL OR s.createdAt <= :to)
""")
    Page<Sale> searchSales(
            @Param("status") Sale.SaleStatus status,
            @Param("customerId") UUID customerId,
            @Param("branchId") UUID branchId,
            @Param("from") LocalDateTime from,
            @Param("to") LocalDateTime to,
            Pageable pageable
    );

    boolean existsByReceiptNo(String receiptNo);

    @Query("""
        select max(s.receiptNo)
        from Sale s
        where s.receiptNo like :prefix%
    """)
    String findMaxReceiptNo(@Param("prefix") String prefix);

    @Modifying
    @Query("""
    delete from Sale s 
    where exists (
        select 1 from SaleLineItem li 
        where li.productVariantId in :variantIds
    )
""")
    void deleteSalesByVariantIds(List<UUID> variantIds);

    @Query("""
        select count(s) > 0
        from Sale s
        join s.lineItems li
        where li.productVariantId in :variantIds
          and s.status = :status
    """)
    boolean existsByVariantIdsAndStatus(
            @Param("variantIds") List<UUID> variantIds,
            @Param("status") Sale.SaleStatus status
    );

    @Query("""
    SELECT 
        FUNCTION('DATEDIFF', CURRENT_DATE, s.createdAt),
        COALESCE(
            SUM(
                s.totalAmount -
                COALESCE(
                    (SELECT SUM(p.amount)
                     FROM Payment p
                     WHERE p.sale = s
                       AND p.status = com.IntegrityTechnologies.business_manager.modules.finance.payment.model.PaymentStatus.SUCCESS),
                0)
            ),
        0)
    FROM Sale s
    JOIN s.lineItems li
    WHERE s.status = com.IntegrityTechnologies.business_manager.modules.finance.sales.base.model.Sale.SaleStatus.COMPLETED
      AND (:branchId IS NULL OR li.branchId = :branchId)
    GROUP BY s.id, s.createdAt
""")
    List<Object[]> arAgingRaw(@Param("branchId") UUID branchId);

    @Query("""
        select max(cast(substring(s.receiptNo, 3) as long))
        from Sale s
        where s.receiptNo like 'R-%'
    """)
    Long findMaxReceiptNumeric();
}