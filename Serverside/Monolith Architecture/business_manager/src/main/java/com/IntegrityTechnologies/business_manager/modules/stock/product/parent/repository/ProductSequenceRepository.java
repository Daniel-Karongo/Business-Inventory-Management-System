package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductSequence;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import jakarta.persistence.LockModeType;
import java.util.Optional;

@Repository
public interface ProductSequenceRepository extends JpaRepository<ProductSequence, Long> {

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("SELECT ps FROM ProductSequence ps WHERE ps.categoryId = :categoryId")
    Optional<ProductSequence> findByCategoryIdForUpdate(@Param("categoryId") Long categoryId);

    // Optional: normal find
    Optional<ProductSequence> findByCategoryId(Long categoryId);
}
