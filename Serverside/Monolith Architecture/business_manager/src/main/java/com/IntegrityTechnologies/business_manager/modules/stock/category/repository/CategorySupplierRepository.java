package com.IntegrityTechnologies.business_manager.modules.stock.category.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.category.model.CategorySupplier;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.CategorySupplierId;
import org.springframework.data.jpa.repository.JpaRepository;

public interface CategorySupplierRepository
        extends JpaRepository<CategorySupplier, CategorySupplierId> {
}