package com.IntegrityTechnologies.business_manager.modules.product.repository;

import com.IntegrityTechnologies.business_manager.modules.product.model.ProductImageAudit;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.SupplierImageAudit;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface ProductImageAuditRepository extends JpaRepository<ProductImageAudit, UUID> {
    List<ProductImageAudit> findByProductId(UUID productId);
}