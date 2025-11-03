package com.IntegrityTechnologies.business_manager.modules.supplier.repository;

import com.IntegrityTechnologies.business_manager.modules.supplier.model.SupplierImage;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface SupplierImageRepository extends JpaRepository<SupplierImage, Long> {
    List<SupplierImage> findBySupplierId(Long supplierId);
}