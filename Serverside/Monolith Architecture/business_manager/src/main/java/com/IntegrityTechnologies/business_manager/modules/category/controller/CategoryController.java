package com.IntegrityTechnologies.business_manager.modules.category.controller;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
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

    // ---------------- SAVE / UPDATE ----------------
    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER','SUPERVISOR')")
    @PostMapping
    public ResponseEntity<CategoryDTO> save(@Valid @RequestBody CategoryDTO dto) {
        return ResponseEntity.ok(categoryService.saveCategory(dto));
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER','SUPERVISOR')")
    @PutMapping("/{id}/recursive")
    public ResponseEntity<CategoryDTO> updateRecursive(
            @PathVariable Long id,
            @Valid @RequestBody CategoryDTO dto) {

        dto.setId(id);
        CategoryDTO updated = categoryService.updateCategoryRecursive(dto);
        return ResponseEntity.ok(updated);
    }

    // ---------------- SINGLE CATEGORY ----------------
    // ---------------- SINGLE CATEGORY FOR SUPERUSER ----------------
    @PreAuthorize("hasRole('SUPERUSER')")
    @GetMapping("/{id}/all")
    public ResponseEntity<CategoryDTO> getCategoryActiveAndDeleted(
            @PathVariable Long id,
            @RequestParam(defaultValue = "tree") String mode) {

        CategoryDTO dto = "flat".equalsIgnoreCase(mode)
                ? categoryService.getCategoryFlat(id)      // includes deleted
                : categoryService.getCategoryTree(id);     // includes deleted

        return ResponseEntity.ok(dto);
    }

    // ---------------- SINGLE CATEGORY FOR OTHER USERS ----------------
    @GetMapping("/{id}/active")
    public ResponseEntity<CategoryDTO> getCategoryActive (
            @PathVariable Long id,
            @RequestParam(defaultValue = "tree") String mode) {

        CategoryDTO dto = "flat".equalsIgnoreCase(mode)
                ? categoryService.getCategoryFlatActive(id)   // only active
                : categoryService.getCategoryTreeActive(id);  // only active

        return ResponseEntity.ok(dto);
    }


    // ---------------- SOFT / HARD DELETE ----------------
    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @DeleteMapping("/{id}")
    public ResponseEntity<ApiResponse> softDelete(@PathVariable Long id) {
        return categoryService.softDelete(id);
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @DeleteMapping("/{id}/hard")
    public ResponseEntity<?> hardDelete(@PathVariable Long id) {
        return categoryService.hardDelete(id);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @PutMapping("/{id}/restore")
    public ResponseEntity<?> restore(@PathVariable Long id) {
        return categoryService.restore(id);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @PutMapping("/{id}/restore-recursive")
    public ResponseEntity<?> restoreCategoryRecursively(@PathVariable Long id) {
        return categoryService.restoreRecursively(id);
    }


    // ---------------- TREE ENDPOINTS ----------------
    @GetMapping("/active/tree")
    public ResponseEntity<List<CategoryDTO>> getActiveTree() {
        return ResponseEntity.ok(categoryService.getAllActiveTree());
    }

    @GetMapping("/deleted/tree")
    public ResponseEntity<List<CategoryDTO>> getDeletedTree() {
        return ResponseEntity.ok(categoryService.getAllDeletedTree());
    }

    @GetMapping("/all/tree")
    public ResponseEntity<List<CategoryDTO>> getAllTree() {
        return ResponseEntity.ok(categoryService.getAllIncludingDeletedTree());
    }

    // ---------------- FLAT ENDPOINTS ----------------
    @GetMapping("/active/flat")
    public ResponseEntity<List<CategoryDTO>> getActiveFlat() {
        return ResponseEntity.ok(categoryService.getAllActiveFlat());
    }

    @GetMapping("/deleted/flat")
    public ResponseEntity<List<CategoryDTO>> getDeletedFlat() {
        return ResponseEntity.ok(categoryService.getAllDeletedFlat());
    }

    @GetMapping("/all/flat")
    public ResponseEntity<List<CategoryDTO>> getAllFlat() {
        return ResponseEntity.ok(categoryService.getAllIncludingDeletedFlat());
    }

    // ---------------- SEARCH ----------------
    @PreAuthorize("hasRole('SUPERUSER')")
    @GetMapping("/all/search")
    public ResponseEntity<List<CategoryDTO>> searchSuperuser(@RequestParam String keyword) {
        List<CategoryDTO> results = categoryService.searchByKeyword(keyword, false); // all categories
        return ResponseEntity.ok(results);
    }

    @GetMapping("/active/search")
    public ResponseEntity<List<CategoryDTO>> searchForUser(@RequestParam String keyword) {
        List<CategoryDTO> results = categoryService.searchByKeyword(keyword, true); // only active
        return ResponseEntity.ok(results);
    }
}