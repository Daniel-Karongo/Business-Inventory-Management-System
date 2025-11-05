package com.IntegrityTechnologies.business_manager.modules.supplier.repository;

import com.IntegrityTechnologies.business_manager.modules.supplier.model.SupplierImage;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface SupplierImageRepository extends JpaRepository<SupplierImage, Long> {
    List<SupplierImage> findBySupplierId(Long supplierId);
    List<SupplierImage> findBySupplierIdAndDeletedFalse(Long supplierId);
    SupplierImage findBySupplierIdAndFileName(Long supplierId, String fileName);
    @Modifying
    @Query("DELETE FROM SupplierImage si WHERE si.supplier.id = :supplierId")
    void deleteAllBySupplierId(@Param("supplierId") Long supplierId);
}