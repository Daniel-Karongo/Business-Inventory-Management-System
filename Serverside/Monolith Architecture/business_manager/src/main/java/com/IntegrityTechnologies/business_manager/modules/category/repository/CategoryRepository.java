package com.IntegrityTechnologies.business_manager.modules.category.repository;

import com.IntegrityTechnologies.business_manager.modules.category.model.Category;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface CategoryRepository extends JpaRepository<Category, Long> {

    @Query("SELECT c FROM Category c") // Includes deleted and active
    List<Category> findAllIncludingDeleted();

    @Query("SELECT c FROM Category c WHERE c.deleted = true")
    List<Category> findDeletedCategories();
}