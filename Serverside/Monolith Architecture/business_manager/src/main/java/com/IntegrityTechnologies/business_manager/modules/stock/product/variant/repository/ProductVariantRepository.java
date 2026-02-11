package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import org.springframework.data.jpa.repository.JpaRepository;
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
}