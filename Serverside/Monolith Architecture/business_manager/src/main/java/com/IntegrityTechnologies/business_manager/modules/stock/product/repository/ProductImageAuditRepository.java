package com.IntegrityTechnologies.business_manager.modules.stock.product.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.model.ProductImageAudit;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface ProductImageAuditRepository extends JpaRepository<ProductImageAudit, UUID> {
    List<ProductImageAudit> findByProductId(UUID productId);
}