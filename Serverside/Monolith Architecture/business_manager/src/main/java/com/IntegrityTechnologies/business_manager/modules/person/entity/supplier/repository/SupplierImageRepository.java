package com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.repository;

import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.SupplierImage;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface SupplierImageRepository extends JpaRepository<SupplierImage, UUID> {
    List<SupplierImage> findBySupplierId(UUID supplierId);
    List<SupplierImage> findBySupplierIdAndDeletedFalse(UUID supplierId);
    List<SupplierImage> findBySupplierIdAndDeletedTrue(UUID supplierId);
    List<SupplierImage> findBySupplier_Id(UUID supplierId);

    List<SupplierImage> findBySupplier_IdAndDeleted(UUID supplierId, Boolean deleted);
    Optional<SupplierImage> findBySupplierIdAndFileName(UUID supplierId, String fileName);

    Optional<SupplierImage> findBySupplierIdAndFileNameAndDeletedFalse(UUID supplierId, String fileName);

    Optional<SupplierImage> findBySupplierIdAndFileNameAndDeletedTrue(UUID supplierId, String fileName);

    @Modifying
    @Query("DELETE FROM SupplierImage si WHERE si.supplier.id = :supplierId")
    void deleteAllBySupplierId(@Param("supplierId") UUID supplierId);

    @Modifying
    @Query("DELETE FROM SupplierImage si WHERE si.supplier.id IN :supplierIds")
    void deleteAllBySupplierIds(@Param("supplierIds") List<UUID> supplierIds);
}