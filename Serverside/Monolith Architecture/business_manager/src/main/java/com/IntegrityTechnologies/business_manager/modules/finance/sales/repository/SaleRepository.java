package com.IntegrityTechnologies.business_manager.modules.finance.sales.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.Sale;
import jakarta.persistence.LockModeType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

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

}