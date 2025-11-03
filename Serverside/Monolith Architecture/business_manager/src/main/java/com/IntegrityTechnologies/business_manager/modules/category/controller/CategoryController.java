package com.IntegrityTechnologies.business_manager.modules.category.controller;

import com.IntegrityTechnologies.business_manager.modules.category.dto.CategoryDTO;
import com.IntegrityTechnologies.business_manager.modules.category.service.CategoryService;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "Categories", description = "Operations related to categories")
@RestController
@RequestMapping("/api/categories")
@RequiredArgsConstructor
public class CategoryController {

    private final CategoryService categoryService;

    /**
     * ✅ Create or update category
     */
    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER', 'SUPERVISOR')")
    @PostMapping
    public ResponseEntity<CategoryDTO> saveCategory(@Valid @RequestBody CategoryDTO dto) {
        return ResponseEntity.ok(categoryService.saveCategory(dto));
    }

    /**
     * ✅ Get single category by ID
     */
    @GetMapping("/{id}")
    public ResponseEntity<CategoryDTO> getCategory(@PathVariable Long id) {
        return ResponseEntity.ok(categoryService.getCategory(id));
    }

    /**
     * ✅ Soft delete (mark as deleted = true)
     */
    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER', 'SUPERVISOR')")
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> softDeleteCategory(@PathVariable Long id) {
        categoryService.softDeleteCategory(id);
        return ResponseEntity.noContent().build();
    }

    /**
     * ✅ Hard delete (permanently remove from DB)
     */
    @PreAuthorize("hasRole('SUPERUSER')")
    @DeleteMapping("/{id}/hard")
    public ResponseEntity<Void> hardDeleteCategory(@PathVariable Long id) {
        categoryService.hardDeleteCategory(id);
        return ResponseEntity.noContent().build();
    }

    /**
     * ✅ View all deleted categories
     */
    @PreAuthorize("hasAnyRole('SUPERUSER')")
    @GetMapping("/deleted")
    public ResponseEntity<List<CategoryDTO>> getDeletedCategories() {
        return ResponseEntity.ok(categoryService.getDeletedCategories());
    }

    /**
     * ✅ View all categories (including deleted)
     */
    @PreAuthorize("hasRole('SUPERUSER')")
    @GetMapping("/all-including-deleted")
    public ResponseEntity<List<CategoryDTO>> getAllIncludingDeleted() {
        return ResponseEntity.ok(categoryService.getAllIncludingDeleted());
    }

    @GetMapping("/all-active")
    public ResponseEntity<List<CategoryDTO>> getAllActiveCategories() {
        return ResponseEntity.ok(categoryService.getAllActiveCategories());
    }

    /**
     * ✅ Restore soft-deleted category
     */
    @PreAuthorize("hasRole('ADMIN')")
    @PutMapping("/{id}/restore")
    public ResponseEntity<Void> restoreCategory(@PathVariable Long id) {
        categoryService.restoreCategory(id);
        return ResponseEntity.noContent().build();
    }
}