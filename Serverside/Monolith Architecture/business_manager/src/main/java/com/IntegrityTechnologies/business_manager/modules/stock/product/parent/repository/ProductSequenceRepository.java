package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductSequence;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;

import jakarta.persistence.LockModeType;
import java.util.Optional;
import java.util.UUID;

public interface ProductSequenceRepository extends JpaRepository<ProductSequence, Long> {

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("""
        SELECT ps
        FROM ProductSequence ps
        WHERE ps.categoryId = :categoryId
          AND ps.tenantId = :tenantId
          AND ps.branchId = :branchId
    """)
    Optional<ProductSequence> findForUpdate(
            @Param("categoryId") Long categoryId,
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );
}