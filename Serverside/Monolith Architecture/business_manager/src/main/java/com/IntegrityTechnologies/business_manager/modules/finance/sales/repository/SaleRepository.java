package com.IntegrityTechnologies.business_manager.modules.finance.sales.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.Sale;
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
            DATEDIFF(CURRENT_DATE, s.createdAt),
            (s.totalAmount - COALESCE(SUM(p.amount),0))
        FROM Sale s
        LEFT JOIN Payment p ON p.sale.id = s.id
        WHERE s.status = 'COMPLETED'
        GROUP BY s.id, s.createdAt, s.totalAmount
    """)
    List<Object[]> arAgingRaw();
}