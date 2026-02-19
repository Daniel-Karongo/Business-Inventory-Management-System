package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface ProductVariantRepository extends JpaRepository<ProductVariant, UUID> {

    List<ProductVariant> findByProduct_Id(UUID productId);

    Optional<ProductVariant> findByProductIdAndClassification(UUID productId, String classification);

    boolean existsByProduct_IdAndClassification(UUID productId, String classification);

    /* =============================
       NEW — READ-ONLY BARCODE ACCESS
       ============================= */

    Optional<ProductVariant> findByBarcode(String barcode);

    List<ProductVariant> findByProductId(UUID productId);
    Optional<ProductVariant> findByProduct_IdAndClassification(
            UUID productId,
            String classification
    );

    Optional<ProductVariant> findBySku(String sku);
    List<ProductVariant> findByProduct_NameIgnoreCaseAndClassificationAndDeletedFalse(
            String productName,
            String classification
    );

    long countByProduct_IdAndDeletedFalse(UUID productId);

    void deleteAllByProduct_Id(UUID productId);

    @Modifying
    @Query("""
    update ProductVariant v
    set v.deleted = true
    where v.product.id = :productId
""")
    void softDeleteByProductId(@Param("productId") UUID productId);

    @Modifying
    @Query("""
    update ProductVariant v
    set v.deleted = false
    where v.product.id = :productId
""")
    void restoreByProductId(@Param("productId") UUID productId);
    @Modifying
    @Query("DELETE FROM ProductVariant v WHERE v.product.id = :productId")
    void deleteByProductId(@Param("productId") UUID productId);
}