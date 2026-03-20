package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.model.ProductVariantPackagingAudit;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface ProductVariantPackagingAuditRepository
        extends JpaRepository<ProductVariantPackagingAudit, UUID> {}
