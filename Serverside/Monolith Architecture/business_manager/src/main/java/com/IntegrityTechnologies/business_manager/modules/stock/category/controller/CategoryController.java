package com.IntegrityTechnologies.business_manager.modules.stock.category.controller;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import com.IntegrityTechnologies.business_manager.common.bulk.BulkRequest;
import com.IntegrityTechnologies.business_manager.common.bulk.BulkResult;
import com.IntegrityTechnologies.business_manager.modules.stock.category.dto.CategoryBulkRow;
import com.IntegrityTechnologies.business_manager.modules.stock.category.dto.CategoryDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.category.service.CategoryBulkService;
import com.IntegrityTechnologies.business_manager.modules.stock.category.service.CategoryService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.dto.SupplierDTO;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;

import java.util.ArrayList;
import java.util.List;

@Tag(name = "Categories")
@RestController
@RequestMapping("/api/categories")
@RequiredArgsConstructor
public class CategoryController {

    private final CategoryService categoryService;
    private final CategoryBulkService bulkService;

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER','SUPERVISOR')")
    @PostMapping("/import")
    public ResponseEntity<BulkResult<CategoryDTO>> importCategories(
            @RequestBody BulkRequest<CategoryBulkRow> request
    ) {
        return ResponseEntity.ok(
                bulkService.importCategories(request)
        );
    }

//    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER','SUPERVISOR')")
//    @PostMapping("/bulk")
//    @Transactional
//    public ResponseEntity<List<CategoryDTO>> saveCategoriesBulk(
//            @Valid @RequestBody List<CategoryDTO> categories
//    ) {
//        List<CategoryDTO> savedCategories = new ArrayList<>();
//        for (CategoryDTO dto : categories) {
//            savedCategories.add(categoryService.saveCategory(dto));
//        }
//        return ResponseEntity.status(HttpStatus.CREATED).body(savedCategories);
//    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER','SUPERVISOR')")
    @PostMapping
    public ResponseEntity<CategoryDTO> save(@Valid @RequestBody CategoryDTO dto) {
        return ResponseEntity.ok(categoryService.saveCategory(dto));
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER','SUPERVISOR')")
    @PatchMapping("/{id}/recursive")
    public ResponseEntity<CategoryDTO> updateRecursive(
            @PathVariable Long id,
            @Valid @RequestBody CategoryDTO dto) {

        dto.setId(id);
        CategoryDTO updated = categoryService.updateCategoryRecursive(dto);
        return ResponseEntity.ok(updated);
    }







    // ---------------- READ OPERATIONS ----------------
    @PreAuthorize("""
    #deleted == false 
    or 
    ( #deleted == true and hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER') )""")
    @GetMapping("/all")
    public ResponseEntity<List<CategoryDTO>> getAllCategories (
            @RequestParam(defaultValue = "tree") String mode,
            @RequestParam(defaultValue = "false") Boolean deleted
    ) {
        return ResponseEntity.ok(categoryService.getAllCategories(mode, deleted));
    }

    @PreAuthorize("""
    #deleted == false 
    or 
    ( #deleted == true and hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER') )""")
    @GetMapping("/search")
    public ResponseEntity<List<CategoryDTO>> searchByKeyword(
            @RequestParam String keyword,
            @RequestParam(required = true,defaultValue = "false") Boolean deleted
    ) {
        List<CategoryDTO> results = categoryService.searchByKeyword(keyword, deleted); // all categories
        return ResponseEntity.ok(results);
    }

    @PreAuthorize("""
    #deleted == false 
    or 
    ( #deleted == true and hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER') )""")
    @GetMapping("/{id}")
    public ResponseEntity<CategoryDTO> getCategory (
            @PathVariable Long id,
            @RequestParam(defaultValue = "tree") String mode,
            @RequestParam(defaultValue = "false") Boolean deleted
    ) {
        return ResponseEntity.ok(categoryService.getCategory(id, mode, deleted));
    }

    @PreAuthorize("""
    #deleted == false 
    or 
    ( #deleted == true and hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER') )""")
    @GetMapping("/{id}/suppliers")
    public ResponseEntity<List<SupplierDTO>> getCategorySuppliers(
            @PathVariable Long id,
            @RequestParam(required = false) Boolean deleted,
            @RequestParam(defaultValue = "true") Boolean strict
    ) {
        return ResponseEntity.ok(categoryService.getCategorySuppliers(id, deleted, strict));
    }







    // ---------------- SOFT / HARD DELETE / RESTORE ----------------
    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @DeleteMapping("/{id}/soft")
    public ResponseEntity<ApiResponse> softDelete(@PathVariable Long id) {
        return categoryService.softDelete(id);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @DeleteMapping("/bulk/soft")
    public ResponseEntity<ApiResponse> softDeleteInBulk(@RequestBody List<Long> categoryIds) {
        return categoryService.softDeleteInBulk(categoryIds);
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @DeleteMapping("/{id}/hard")
    public ResponseEntity<ApiResponse> hardDelete(@PathVariable Long id) {
        return categoryService.hardDelete(id);
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @DeleteMapping("/bulk/hard")
    public ResponseEntity<ApiResponse> hardDeleteInBulk(@RequestBody List<Long> categoryIds) {
        return categoryService.hardDeleteInBulk(categoryIds);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @PutMapping("/{id}/restore")
    public ResponseEntity<ApiResponse> restore(@PathVariable Long id) {
        return categoryService.restore(id);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @PutMapping("/restore/bulk")
    public ResponseEntity<ApiResponse> restoreInBulk(@RequestBody List<Long> categoryIds) {
        return categoryService.restoreInBulk(categoryIds);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @PutMapping("/{id}/restore-recursive")
    public ResponseEntity<ApiResponse> restoreCategoryRecursively(@PathVariable Long id) {
        return categoryService.restoreRecursively(id);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @PutMapping("/restore-recursive/bulk")
    public ResponseEntity<ApiResponse> restoreCategoriesRecursivelyInBulk(@RequestBody List<Long> categoryIds) {
        return categoryService.restoreCategoriesRecursivelyInBulk(categoryIds);
    }
}