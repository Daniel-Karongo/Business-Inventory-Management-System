package com.IntegrityTechnologies.business_manager.modules.product.repository;

import com.IntegrityTechnologies.business_manager.modules.product.model.Product;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ProductRepository extends JpaRepository<Product, Long>, JpaSpecificationExecutor<Product> {

    // Existing methods
    Page<Product> findByCategoryIdAndNameContainingIgnoreCase(Long categoryId, String name, Pageable pageable);
    Page<Product> findByCategoryId(Long categoryId, Pageable pageable);
    Page<Product> findByNameContainingIgnoreCase(String name, Pageable pageable);

    // New convenience methods
    List<Product> findByDeletedFalse();
    List<Product> findByDeletedTrue();
}