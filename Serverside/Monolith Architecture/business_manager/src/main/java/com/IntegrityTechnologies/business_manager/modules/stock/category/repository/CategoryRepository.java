package com.IntegrityTechnologies.business_manager.modules.stock.category.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface CategoryRepository extends JpaRepository<Category, Long> {

    Optional<Category> findByNameIgnoreCase(String name);

    @Query("SELECT c FROM Category c WHERE c.deleted = true")
    List<Category> findAllDeleted();

    // FLAT QUERIES
    @Query("SELECT c FROM Category c WHERE c.deleted = false")
    List<Category> findAllActiveFlat();

    @Query("SELECT c FROM Category c WHERE c.deleted = true")
    List<Category> findAllDeletedFlat();

    @Query("SELECT c FROM Category c")
    List<Category> findAllIncludingDeletedFlat();

    Optional<Category> findByIdAndDeletedFalse(Long id);
    Optional<Category> findByIdAndDeletedTrue(Long id);


    @Query("""
        SELECT c FROM Category c 
        WHERE c.deleted = false AND 
        (LOWER(c.name) LIKE LOWER(CONCAT('%', :keyword, '%')) 
         OR LOWER(c.description) LIKE LOWER(CONCAT('%', :keyword, '%')))
        """)
    List<Category> searchActive(@Param("keyword") String keyword);

    // Search all categories (active + deleted)
    @Query("""
        SELECT c FROM Category c
        WHERE LOWER(c.name) LIKE LOWER(CONCAT('%', :keyword, '%'))
        OR LOWER(c.description) LIKE LOWER(CONCAT('%', :keyword, '%'))
        """)
    List<Category> searchAll(@Param("keyword") String keyword);

    // Detach all suppliers from category
    @Modifying
    @Query(value = "DELETE FROM supplier_categories WHERE category_id = :categoryId", nativeQuery = true)
    void detachSuppliers(@Param("categoryId") Long categoryId);

    // Detach all products from category
    @Modifying
    @Query(value = "UPDATE products SET category_id = NULL WHERE category_id = :categoryId", nativeQuery = true)
    void detachProducts(@Param("categoryId") Long categoryId);

    // Get direct subcategory IDs
    @Query(value = "SELECT id FROM categories WHERE parent_id = :categoryId", nativeQuery = true)
    List<Long> findSubcategoryIds(@Param("categoryId") Long categoryId);

    // Delete category by ID
    @Modifying
    @Query(value = "DELETE FROM categories WHERE id = :categoryId", nativeQuery = true)
    void deleteCategoryById(@Param("categoryId") Long categoryId);

    boolean existsByNameIgnoreCase(String categoryName);
    boolean existsByNameIgnoreCaseAndParent_Id(String name, Long parentId);
    @Modifying
    @Query("UPDATE Category c SET c.deleted = true, c.deletedAt = CURRENT_TIMESTAMP WHERE c.path LIKE CONCAT(:path, '%')")
    void softDeleteByPath(@Param("path") String path);

    @Modifying
    @Query("UPDATE Category c SET c.deleted = false, c.deletedAt = NULL WHERE c.path LIKE CONCAT(:path, '%')")
    void restoreByPath(@Param("path") String path);

    @Modifying
    @Query("DELETE FROM Category c WHERE c.path LIKE CONCAT(:path, '%')")
    void hardDeleteByPath(@Param("path") String path);

    @Query("SELECT c FROM Category c WHERE c.path LIKE CONCAT(:path, '%')")
    List<Category> findSubtree(@Param("path") String path);

    @Modifying
    @Query("""
        UPDATE Category c
        SET c.path = CONCAT(:newPrefix, SUBSTRING(c.path, LENGTH(:oldPrefix) + 1))
        WHERE c.path LIKE CONCAT(:oldPrefix, '%')
    """)
    void rewriteSubtreePath(@Param("oldPrefix") String oldPrefix,
                            @Param("newPrefix") String newPrefix);

    @Query("""
        SELECT c FROM Category c
        WHERE (:deleted IS NULL OR c.deleted = :deleted)
        ORDER BY c.path
    """)
    List<Category> findAllOrderedByPath(@Param("deleted") Boolean deleted);
}