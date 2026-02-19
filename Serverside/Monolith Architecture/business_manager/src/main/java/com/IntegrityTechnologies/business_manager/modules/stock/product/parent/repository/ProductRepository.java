package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import jakarta.persistence.LockModeType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface ProductRepository extends JpaRepository<Product, UUID>, JpaSpecificationExecutor<Product> {
    boolean existsByNameIgnoreCase(String name);
    Page<Product> findByCategoryIdAndNameContainingIgnoreCase(Long categoryId, String name, Pageable pageable);
    Page<Product> findByCategoryId(Long categoryId, Pageable pageable);
    Page<Product> findByNameContainingIgnoreCase(String name, Pageable pageable);

    Optional<Product> findBySku(String sku);
    List<Product> findAllByIdInAndDeletedFalse(List<UUID> ids);
    boolean existsBySku(String sku);
    boolean existsByName(String name);

    List<Product> findByDeletedFalse();
    List<Product> findByDeletedTrue();
    List<Product> findAllBySuppliers_Id(UUID supplierId);
    List<Product> findAllByCategory_Id(Long categoryId);
    List<Product> findAllByCategory_IdIn(List<Long> categoryIds);
    Optional<Product> findByIdAndDeletedFalse(UUID productId);
    Optional<Product> findByNameIgnoreCase(String productName);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("select p from Product p where p.id = :id")
    Optional<Product> findForUpdate(@Param("id") UUID id);

    @Modifying
    @Query(value = "DELETE FROM products_suppliers WHERE product_id = :productId", nativeQuery = true)
    void detachSuppliers(@Param("productId") UUID productId);
}