package com.IntegrityTechnologies.business_manager.modules.stock.category.controller;

import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkRequest;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkResult;
import com.IntegrityTechnologies.business_manager.modules.stock.category.dto.CategoryBulkRow;
import com.IntegrityTechnologies.business_manager.modules.stock.category.dto.CategoryDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.category.service.CategoryBulkService;
import com.IntegrityTechnologies.business_manager.modules.stock.category.service.CategoryService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.dto.SupplierDTO;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.*;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "Categories")
@RestController
@RequestMapping("/api/categories")
@RequiredArgsConstructor
@TenantUserOnly
public class CategoryController {

    private final CategoryService categoryService;
    private final CategoryBulkService bulkService;

    /* ==============================
       IMPORT
       ============================== */

    @TenantSupervisorOnly
    @PostMapping("/import")
    public ResponseEntity<BulkResult<CategoryDTO>> importCategories(
            @RequestBody BulkRequest<CategoryBulkRow> request
    ) {
        return ResponseEntity.ok(
                bulkService.importCategories(request)
        );
    }

    /* ==============================
       CREATE
       ============================== */

    @TenantSupervisorOnly
    @PostMapping
    public ResponseEntity<CategoryDTO> save(@Valid @RequestBody CategoryDTO dto) {
        return ResponseEntity.ok(categoryService.saveCategory(dto));
    }

    @TenantSupervisorOnly
    @PatchMapping("/{id}/recursive")
    public ResponseEntity<CategoryDTO> updateRecursive(
            @PathVariable Long id,
            @Valid @RequestBody CategoryDTO dto
    ) {

        dto.setId(id);

        CategoryDTO updated =
                categoryService.updateCategoryRecursive(dto);

        return ResponseEntity.ok(updated);
    }

    /* ==============================
       READ OPERATIONS
       ============================== */

    @GetMapping("/all")
    public ResponseEntity<List<CategoryDTO>> getAllCategories(
            @RequestParam(defaultValue = "tree") String mode,
            @RequestParam(defaultValue = "false") Boolean deleted
    ) {

        return ResponseEntity.ok(
                categoryService.getAllCategories(mode, deleted)
        );
    }

    @GetMapping("/search")
    public ResponseEntity<List<CategoryDTO>> searchByKeyword(
            @RequestParam String keyword,
            @RequestParam(required = true, defaultValue = "false") Boolean deleted
    ) {

        List<CategoryDTO> results =
                categoryService.searchByKeyword(keyword, deleted);

        return ResponseEntity.ok(results);
    }

    @GetMapping("/{id}")
    public ResponseEntity<CategoryDTO> getCategory(
            @PathVariable Long id,
            @RequestParam(defaultValue = "tree") String mode,
            @RequestParam(defaultValue = "false") Boolean deleted
    ) {

        return ResponseEntity.ok(
                categoryService.getCategory(id, mode, deleted)
        );
    }

    @GetMapping("/{id}/suppliers")
    public ResponseEntity<List<SupplierDTO>> getCategorySuppliers(
            @PathVariable Long id,
            @RequestParam(required = false) Boolean deleted,
            @RequestParam(defaultValue = "true") Boolean strict
    ) {

        return ResponseEntity.ok(
                categoryService.getCategorySuppliers(id, deleted, strict)
        );
    }

    /* ==============================
       DELETE / RESTORE
       ============================== */

    @TenantManagerOnly
    @DeleteMapping("/{id}/soft")
    public ResponseEntity<ApiResponse> softDelete(@PathVariable Long id) {
        return categoryService.softDelete(id);
    }

    @TenantManagerOnly
    @DeleteMapping("/bulk/soft")
    public ResponseEntity<ApiResponse> softDeleteInBulk(
            @RequestBody List<Long> categoryIds
    ) {
        return categoryService.softDeleteInBulk(categoryIds);
    }

    @PlatformAdminOnly
    @DeleteMapping("/{id}/hard")
    public ResponseEntity<ApiResponse> hardDelete(@PathVariable Long id) {
        return categoryService.hardDelete(id);
    }

    @PlatformAdminOnly
    @DeleteMapping("/bulk/hard")
    public ResponseEntity<ApiResponse> hardDeleteInBulk(
            @RequestBody List<Long> categoryIds
    ) {
        return categoryService.hardDeleteInBulk(categoryIds);
    }

    @TenantManagerOnly
    @PutMapping("/{id}/restore")
    public ResponseEntity<ApiResponse> restore(@PathVariable Long id) {
        return categoryService.restore(id);
    }

    @TenantManagerOnly
    @PutMapping("/restore/bulk")
    public ResponseEntity<ApiResponse> restoreInBulk(
            @RequestBody List<Long> categoryIds
    ) {
        return categoryService.restoreInBulk(categoryIds);
    }

    @TenantManagerOnly
    @PutMapping("/{id}/restore-recursive")
    public ResponseEntity<ApiResponse> restoreCategoryRecursively(
            @PathVariable Long id
    ) {
        return categoryService.restoreRecursively(id);
    }

    @TenantManagerOnly
    @PutMapping("/restore-recursive/bulk")
    public ResponseEntity<ApiResponse> restoreCategoriesRecursivelyInBulk(
            @RequestBody List<Long> categoryIds
    ) {
        return categoryService.restoreCategoriesRecursivelyInBulk(categoryIds);
    }
}