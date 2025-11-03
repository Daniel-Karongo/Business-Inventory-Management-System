package com.IntegrityTechnologies.business_manager.modules.category.repository;

import com.IntegrityTechnologies.business_manager.modules.category.model.Category;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface CategoryRepository extends JpaRepository<Category, Long> {

    Optional<Category> findByNameIgnoreCase(String name);

    // TREE QUERIES
    @Query("SELECT DISTINCT c FROM Category c LEFT JOIN FETCH c.subcategories s WHERE c.deleted = false")
    List<Category> findAllActiveWithSubcategories();

    @Query("SELECT c FROM Category c WHERE c.deleted = true")
    List<Category> findAllDeleted();

    @Query("SELECT DISTINCT c FROM Category c LEFT JOIN FETCH c.subcategories s")
    List<Category> findAllIncludingDeletedWithSubcategories();

    @Query("SELECT c FROM Category c LEFT JOIN FETCH c.subcategories WHERE c.id = :id")
    Optional<Category> findByIdWithSubcategories(@Param("id") Long id);

    @Query("SELECT c FROM Category c LEFT JOIN FETCH c.subcategories WHERE c.deleted = false AND c.id = :id")
    Optional<Category> findByIdWithSubcategoriesAndActive(@Param("id") Long id);


    // FLAT QUERIES
    @Query("SELECT c FROM Category c WHERE c.deleted = false")
    List<Category> findAllActiveFlat();

    @Query("SELECT c FROM Category c WHERE c.deleted = true")
    List<Category> findAllDeletedFlat();

    @Query("SELECT c FROM Category c")
    List<Category> findAllIncludingDeletedFlat();

    Optional<Category> findByIdAndDeletedFalse(Long id);


    // SEARCH
    // Search only active
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
}