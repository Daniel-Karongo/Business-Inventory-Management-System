package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductImage;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.UUID;

@Repository
public interface ProductImageRepository extends JpaRepository<ProductImage, UUID> {

    // Correct property path
    List<ProductImage> findByProduct_Id(UUID productId);

    @Modifying

    @Transactional
    @Query("update ProductImage i set i.deleted = true where i.product.id = :productId")
    void softDeleteByProductId(@Param("productId") UUID productId);

    @Modifying

    @Transactional
    @Query("""
        update ProductImage i
        set i.deleted = false
        where i.product.id = :productId
          and i.deletedIndependently = false
    """)
    void restoreByProductId(@Param("productId") UUID productId);
    @Modifying
    @Query("DELETE FROM ProductImage pi WHERE pi.product.id = :productId")
    void deleteByProductId(@Param("productId") UUID productId);

}