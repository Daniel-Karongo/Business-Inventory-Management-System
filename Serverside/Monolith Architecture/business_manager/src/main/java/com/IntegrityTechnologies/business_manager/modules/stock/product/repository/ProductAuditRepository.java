package com.IntegrityTechnologies.business_manager.modules.stock.product.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.model.ProductAudit;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface ProductAuditRepository extends JpaRepository<ProductAudit, UUID> {
    List<ProductAudit> findByProductIdOrderByTimestampDesc(UUID productId);
}