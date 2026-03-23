package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.dto.VariantScanProjection;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.*;

@Repository
public interface ProductVariantRepository extends JpaRepository<ProductVariant, UUID> {

    @Query("""
    SELECT v FROM ProductVariant v
    WHERE v.id = :id
      AND v.deleted = :deleted
      AND v.tenantId = :tenantId
      AND v.branchId = :branchId
""")
    Optional<ProductVariant> findByIdSafe(
            @Param("id") UUID id,
            @Param("deleted") boolean deleted,
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );

    @Query("""
    SELECT v FROM ProductVariant v
    WHERE v.product.id = :productId
      AND v.deleted = :deleted
      AND v.tenantId = :tenantId
      AND v.branchId = :branchId
""")
    List<ProductVariant> findByProduct_IdSafe(
            @Param("productId") UUID productId,
            @Param("deleted") boolean deleted,
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );

    // ✅ SAFE SCAN QUERY (fetch join for performance)
    @Query("""
        SELECT v FROM ProductVariant v
        JOIN FETCH v.product p
        WHERE v.tenantId = :tenantId
          AND v.branchId = :branchId
          AND v.deleted = false
          AND (v.barcode = :value OR v.sku = :value)
    """)
    Optional<ProductVariant> findForScan(
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId,
            @Param("value") String value
    );

    boolean existsByTenantIdAndBranchIdAndSku(
            UUID tenantId,
            UUID branchId,
            String sku
    );

    List<ProductVariant> findByTenantIdAndBranchIdAndProduct_Id(
            UUID tenantId,
            UUID branchId,
            UUID productId
    );

    Optional<ProductVariant> findByTenantIdAndBranchIdAndId(
            UUID tenantId,
            UUID branchId,
            UUID id
    );

    Optional<ProductVariant> findByTenantIdAndBranchIdAndProduct_IdAndClassification(
            UUID tenantId,
            UUID branchId,
            UUID productId,
            String classification
    );

    boolean existsByTenantIdAndBranchIdAndProduct_IdAndClassification(
            UUID tenantId,
            UUID branchId,
            UUID productId,
            String classification
    );

    long countByTenantIdAndBranchIdAndProduct_IdAndDeletedFalse(
            UUID tenantId,
            UUID branchId,
            UUID productId
    );

    @Modifying
    @Query("""
        update ProductVariant v
        set v.deleted = true
        where v.product.id = :productId
          and v.tenantId = :tenantId
          and v.branchId = :branchId
    """)
    void softDeleteByProductId(
            @Param("productId") UUID productId,
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );

    @Modifying
    @Query("""
        update ProductVariant v
        set v.deleted = false
        where v.product.id = :productId
          and v.tenantId = :tenantId
          and v.branchId = :branchId
    """)
    void restoreByProductId(
            @Param("productId") UUID productId,
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );

    @Modifying
    @Query("""
        delete from ProductVariant v
        where v.product.id = :productId
          and v.tenantId = :tenantId
          and v.branchId = :branchId
    """)
    void deleteByProductId(
            @Param("productId") UUID productId,
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );

    @Query("""
    SELECT 
        p.id as productId,
        p.name as productName,
    
        v.id as variantId,
        v.classification as classification,
        v.sku as sku,
        v.barcode as barcode,
    
        v.branchId as branchId,
        COALESCE(i.quantityOnHand, 0) as quantityOnHand
    
    FROM ProductVariant v
    JOIN v.product p
    LEFT JOIN InventoryItem i 
        ON i.productVariant.id = v.id
        AND i.tenantId = :tenantId
        AND i.branchId = :branchId
    
    WHERE v.tenantId = :tenantId
      AND v.branchId = :branchId
      AND v.deleted = false
      AND (v.barcode = :value OR v.sku = :value)
    """)
    Optional<VariantScanProjection> scanProjection(
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId,
            @Param("value") String value
    );

    Optional<ProductVariant> findByTenantIdAndBranchIdAndSkuAndDeletedFalse(
            UUID tenantId,
            UUID branchId,
            String sku
    );

    @Query("""
    SELECT v FROM ProductVariant v
    JOIN v.product p
    WHERE LOWER(p.name) = LOWER(:name)
      AND v.classification = :classification
      AND v.deleted = false
      AND v.tenantId = :tenantId
""")
    List<ProductVariant> findByProductNameAndClassificationSafe(
            @Param("name") String name,
            @Param("classification") String classification,
            @Param("tenantId") UUID tenantId
    );

    @Query("""
        SELECT v FROM ProductVariant v
        JOIN FETCH v.product p
        WHERE v.tenantId = :tenantId
          AND v.branchId = :branchId
          AND v.deleted = false
          AND (
                LOWER(v.sku) LIKE LOWER(CONCAT('%', :search, '%'))
             OR LOWER(p.name) LIKE LOWER(CONCAT('%', :search, '%'))
          )
    """)
    List<ProductVariant> searchSellable(
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId,
            @Param("search") String search
    );

    @Query("""
        SELECT v FROM ProductVariant v
        JOIN FETCH v.product p
        WHERE v.tenantId = :tenantId
          AND v.branchId = :branchId
          AND v.deleted = false
          AND (
                :search IS NULL OR
                LOWER(v.sku) LIKE LOWER(CONCAT('%', :search, '%')) OR
                LOWER(p.name) LIKE LOWER(CONCAT('%', :search, '%'))
          )
    """)
    Page<ProductVariant> searchSellablePaged(
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId,
            @Param("search") String search,
            Pageable pageable
    );
}