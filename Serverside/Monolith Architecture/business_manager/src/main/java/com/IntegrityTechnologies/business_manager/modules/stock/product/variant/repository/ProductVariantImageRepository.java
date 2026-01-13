package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariantImage;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface ProductVariantImageRepository extends JpaRepository<ProductVariantImage, UUID> {

    List<ProductVariantImage> findByVariant_IdAndDeletedFalse(UUID variantId);

    List<ProductVariantImage> findByVariant_Id(UUID variantId);

    boolean existsByVariant_IdAndFileName(UUID id, String fileName);
}