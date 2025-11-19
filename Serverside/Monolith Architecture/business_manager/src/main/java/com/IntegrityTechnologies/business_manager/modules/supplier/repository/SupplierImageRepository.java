package com.IntegrityTechnologies.business_manager.modules.supplier.repository;

import com.IntegrityTechnologies.business_manager.modules.supplier.model.SupplierImage;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface SupplierImageRepository extends JpaRepository<SupplierImage, UUID> {
    List<SupplierImage> findBySupplierId(UUID supplierId);
    List<SupplierImage> findBySupplierIdAndDeletedFalse(UUID supplierId);
    SupplierImage findBySupplierIdAndFileName(UUID supplierId, String fileName);
    @Modifying
    @Query("DELETE FROM SupplierImage si WHERE si.supplier.id = :supplierId")
    void deleteAllBySupplierId(@Param("supplierId") UUID supplierId);

    @Modifying
    @Query("DELETE FROM SupplierImage si WHERE si.supplier.id IN :supplierIds")
    void deleteAllBySupplierIds(@Param("supplierIds") List<UUID> supplierIds);
}