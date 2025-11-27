package com.IntegrityTechnologies.business_manager.modules.stock.product.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.model.Product;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface ProductRepository extends JpaRepository<Product, UUID>, JpaSpecificationExecutor<Product> {

    Page<Product> findByCategoryIdAndNameContainingIgnoreCase(Long categoryId, String name, Pageable pageable);
    Page<Product> findByCategoryId(Long categoryId, Pageable pageable);
    Page<Product> findByNameContainingIgnoreCase(String name, Pageable pageable);

    Optional<Product> findBySku(String sku);
    List<Product> findAllByIdInAndDeletedFalse(List<UUID> ids);
    Optional<Product> findByBarcode(String barcode);
    Optional<Product> findByBarcodeAndDeletedFalse(String barcode);
    boolean existsBySku(String sku);
    boolean existsByBarcode(String barcode);
    boolean existsByName(String name);

    List<Product> findByDeletedFalse();
    List<Product> findByDeletedTrue();
    List<Product> findAllBySuppliers_Id(UUID supplierId);
    List<Product> findAllByCategory_Id(Long categoryId);
    List<Product> findAllByCategory_IdIn(List<Long> categoryIds);



}