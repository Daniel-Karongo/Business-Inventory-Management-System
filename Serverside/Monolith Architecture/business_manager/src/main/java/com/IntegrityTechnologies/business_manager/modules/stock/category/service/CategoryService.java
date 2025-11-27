package com.IntegrityTechnologies.business_manager.modules.stock.category.service;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import com.IntegrityTechnologies.business_manager.exception.CategoryNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.stock.category.dto.CategoryDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.category.mapper.CategoryMapper;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.stock.category.repository.CategoryRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.dto.SupplierDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.mapper.SupplierMapper;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional
public class CategoryService {

    private final CategoryRepository categoryRepository;
    private final CategoryMapper categoryMapper;
    private final SupplierMapper supplierMapper;

    // ---------------- SAVE / UPDATE ----------------
    public CategoryDTO saveCategory(CategoryDTO dto) {
        Category category = dto.getId() != null
                ? categoryRepository.findById(dto.getId()).orElse(new Category())
                : new Category();

        categoryRepository.findByNameIgnoreCase(dto.getName())
                .ifPresent(existing -> {
                    if (!existing.getId().equals(dto.getId())) {
                        throw new IllegalArgumentException("Category name already exists: " + dto.getName());
                    }
                });

        categoryMapper.updateEntityFromDTO(dto, category);

        if (dto.getParentId() != null) {
            if (dto.getParentId().equals(dto.getId()))
                throw new IllegalArgumentException("Category cannot be its own parent");

            Category parent = categoryRepository.findById(dto.getParentId())
                    .orElseThrow(() -> new CategoryNotFoundException("Parent category not found"));

            if (isChildOf(parent, category))
                throw new IllegalArgumentException("Cannot set parent: would create cycle");

            category.setParent(parent);
        } else {
            category.setParent(null);
        }

        category = categoryRepository.save(category);

        CategoryDTO result = categoryMapper.toDTO(category);
        result.setSubcategories(
                category.getSubcategories() != null
                        ? category.getSubcategories().stream().map(categoryMapper::toDTO).collect(Collectors.toList())
                        : List.of()
        );
        return result;
    }

    @Transactional
    public CategoryDTO updateCategoryRecursive(CategoryDTO dto) {
        return updateCategoryRecursive(dto, false);
    }

    private CategoryDTO updateCategoryRecursive(CategoryDTO dto, boolean parentDeleted) {
        Category category = categoryRepository.findById(dto.getId())
                .orElseThrow(() -> new CategoryNotFoundException("Category not found"));

        // Check for duplicate name
        categoryRepository.findByNameIgnoreCase(dto.getName())
                .ifPresent(existing -> {
                    if (!existing.getId().equals(dto.getId())) {
                        throw new IllegalArgumentException("Category name already exists: " + dto.getName());
                    }
                });

        categoryMapper.updateEntityFromDTO(dto, category);

        // Handle parent
        if (dto.getParentId() != null) {
            if (dto.getParentId().equals(dto.getId()))
                throw new IllegalArgumentException("Category cannot be its own parent");

            Category parent = categoryRepository.findById(dto.getParentId())
                    .orElseThrow(() -> new CategoryNotFoundException("Parent category not found"));

            if (isChildOf(parent, category))
                throw new IllegalArgumentException("Cannot set parent: would create cycle");

            category.setParent(parent);

            // Propagate deleted from parent
            if (parent.isDeleted()) {
                category.setDeleted(true);
                category.setDeletedAt(LocalDateTime.now());
            }
        } else {
            category.setParent(null);
        }

        // If parentDeleted flag is true, override
        if (parentDeleted) {
            category.setDeleted(true);
            category.setDeletedAt(LocalDateTime.now());
        }

        category = categoryRepository.save(category);

        // ------------------ Propagate to all existing children in DB ------------------
        if (category.getSubcategories() != null && !category.getSubcategories().isEmpty()) {
            for (Category child : category.getSubcategories()) {
                updateChildDeletedState(child, category.isDeleted());
            }
        }

        CategoryDTO result = categoryMapper.toDTO(category);
        result.setSubcategories(
                category.getSubcategories() != null
                        ? category.getSubcategories().stream().map(categoryMapper::toDTO).collect(Collectors.toList())
                        : List.of()
        );
        return result;
    }

    /**
     * Recursively propagate deleted/restored state to children from the parent.
     */
    private void updateChildDeletedState(Category child, boolean parentDeleted) {
        boolean changed = false;
        if (parentDeleted && !child.isDeleted()) {
            child.setDeleted(true);
            child.setDeletedAt(LocalDateTime.now());
            changed = true;
        } else if (!parentDeleted && child.isDeleted()) {
            child.setDeleted(false);
            child.setDeletedAt(null);
            changed = true;
        }

        if (changed) {
            categoryRepository.save(child);
        }

        if (child.getSubcategories() != null && !child.getSubcategories().isEmpty()) {
            for (Category sub : child.getSubcategories()) {
                updateChildDeletedState(sub, parentDeleted);
            }
        }
    }

    private boolean isChildOf(Category potentialParent, Category category) {
        if (potentialParent == null) return false;
        if (potentialParent.getId().equals(category.getId())) return true;
        return isChildOf(potentialParent.getParent(), category);
    }









    // ---------------- TREE MODE ----------------
    private CategoryDTO buildCategoryTree(Category category) {
        CategoryDTO dto = categoryMapper.toDTO(category);
        dto.setSubcategories(category.getSubcategories() != null
                ? category.getSubcategories().stream().map(this::buildCategoryTree).collect(Collectors.toList())
                : new ArrayList<>());
        return dto;
    }

    private List<CategoryDTO> buildTree(List<Category> categories) {
        // Step 1: Map all categories to DTOs
        Map<Long, CategoryDTO> dtoMap = categories.stream()
                .map(categoryMapper::toDTO)
                .collect(Collectors.toMap(CategoryDTO::getId, dto -> dto));

        // Step 2: Group children by parent ID
        Map<Long, List<CategoryDTO>> childrenMap = categories.stream()
                .filter(c -> c.getParent() != null)
                .map(categoryMapper::toDTO)
                .collect(Collectors.groupingBy(CategoryDTO::getParentId));

        // Step 3: Attach children recursively
        dtoMap.values().forEach(dto -> {
            List<CategoryDTO> children = childrenMap.getOrDefault(dto.getId(), new ArrayList<>());
            dto.setSubcategories(children);
        });

        // Step 4: Return only root nodes (parent == null OR parent not in deleted list)
        return dtoMap.values().stream()
                .filter(dto -> dto.getParentId() == null || !dtoMap.containsKey(dto.getParentId()))
                .toList();
    }

    public List<CategoryDTO> getAllCategories(String mode, Boolean deleted) {
        if("tree".equals(mode))
            return getAllCategoriesTree(deleted);
        else
            return getAllCategoriesFlat(deleted);
    }

    public List<CategoryDTO> getAllCategoriesTree(Boolean deleted) {
        if(Boolean.FALSE.equals(deleted))
            return buildTree(categoryRepository.findAllActiveWithSubcategories());
        else if(Boolean.TRUE.equals(deleted))
            return buildTree(categoryRepository.findAllDeleted());
        else
            return buildTree(categoryRepository.findAllIncludingDeletedWithSubcategories());
    }

    public List<CategoryDTO> getAllCategoriesFlat(Boolean deleted) {
        if(Boolean.FALSE.equals(deleted))
            return categoryRepository.findAllActiveFlat().stream().map(categoryMapper::toDTO).collect(Collectors.toList());
        else if(Boolean.TRUE.equals(deleted))
            return categoryRepository.findAllDeletedFlat().stream().map(categoryMapper::toDTO).collect(Collectors.toList());
        else
            return categoryRepository.findAllIncludingDeletedFlat().stream().map(categoryMapper::toDTO).collect(Collectors.toList());
    }

    // ---------------- SINGLE CATEGORY ----------------

    public List<SupplierDTO> getCategorySuppliers(Long id, Boolean deleted, Boolean strict) {

        Category category = categoryRepository.findById(id)
                .orElseThrow(() -> new CategoryNotFoundException("Category not found: " + id));

        // Collect suppliers (strict OR including parents)
        Set<Supplier> suppliers = new HashSet<>();

        if (Boolean.TRUE.equals(strict)) {
            // Strict → only this category
            suppliers.addAll(category.getSuppliers());
        } else {
            // Not strict → include all parent categories' suppliers as well
            Category current = category;
            while (current != null) {
                suppliers.addAll(current.getSuppliers());
                current = current.getParent();
            }
        }

        // Apply deleted filter
        List<Supplier> filtered = suppliers.stream()
                .filter(s ->
                        deleted == null
                                ? true
                                : deleted
                                ? Boolean.TRUE.equals(s.getDeleted())
                                : Boolean.FALSE.equals(s.getDeleted())
                )
                .toList();

        return supplierMapper.toDTOList(filtered);
    }

    public CategoryDTO getCategory(Long id, String mode, Boolean deleted) {
        if("tree".equals(mode))
            return getCategoryTree(id, deleted);
        else
            return getCategoryFlat(id, deleted);
    }

    public CategoryDTO getCategoryTree(Long id, Boolean deleted) {
        if(Boolean.FALSE.equals(deleted)) {
            Category category = categoryRepository.findByIdWithSubcategoriesAndActive(id)
                    .orElseThrow(() -> new CategoryNotFoundException("Category not found or inactive"));
            return buildActiveCategoryTree(category);
        } else if(Boolean.TRUE.equals(deleted)) {
            Category category = categoryRepository.findByIdWithSubcategoriesAndDeleted(id)
                    .orElseThrow(() -> new CategoryNotFoundException("Category not found or inactive"));
            return buildActiveCategoryTree(category);
        }else {
            Category category = categoryRepository.findByIdWithSubcategories(id)
                    .orElseThrow(() -> new CategoryNotFoundException("Category not found"));
            return buildCategoryTree(category);
        }
    }

    public CategoryDTO getCategoryFlat(Long id, Boolean deleted) {
        if(Boolean.FALSE.equals(deleted)) {
            Category category = categoryRepository.findByIdAndDeletedFalse(id)
                    .orElseThrow(() -> new CategoryNotFoundException("Category not found or inactive"));
            CategoryDTO dto = categoryMapper.toDTO(category);
            dto.setSubcategories(null); // flat
            return dto;
        } else if(Boolean.TRUE.equals(deleted)) {
            Category category = categoryRepository.findByIdAndDeletedTrue(id)
                    .orElseThrow(() -> new CategoryNotFoundException("Category not found or inactive"));
            CategoryDTO dto = categoryMapper.toDTO(category);
            dto.setSubcategories(null);
            return dto;
        } else {
            Category category = categoryRepository.findById(id)
                    .orElseThrow(() -> new CategoryNotFoundException("Category not found"));
            CategoryDTO dto = categoryMapper.toDTO(category);
            dto.setSubcategories(null);
            return dto;
        }
    }

    // Recursive helper for active categories only
    private CategoryDTO buildActiveCategoryTree(Category category) {
        CategoryDTO dto = categoryMapper.toDTO(category);
        if (category.getSubcategories() != null && !category.getSubcategories().isEmpty()) {
            List<CategoryDTO> subs = category.getSubcategories().stream()
                    .filter(sub -> !sub.isDeleted())
                    .map(this::buildActiveCategoryTree)
                    .toList();
            dto.setSubcategories(subs);
        } else {
            dto.setSubcategories(List.of());
        }
        return dto;
    }








    // ---------------- DELETE / RESTORE ----------------

    // --------------------- SOFT DELETE ---------------------

    /** Internal helper: collect category + all subcategories */
    private void collectCategoryAndChildren(Category category, List<Category> collector) {
        collector.add(category);
        if (category.getSubcategories() != null) {
            category.getSubcategories().forEach(sub -> collectCategoryAndChildren(sub, collector));
        }
    }

    /** Soft delete a single category */
    @Transactional
    public ResponseEntity<ApiResponse> softDelete(Long id) {
        Category category = categoryRepository.findById(id)
                .orElseThrow(() -> new CategoryNotFoundException("Category not found: " + id));

        List<Map<String, Object>> modified = new ArrayList<>();
        List<Category> allCategories = new ArrayList<>();
        collectCategoryAndChildren(category, allCategories);

        for (Category cat : allCategories) {
            cat.setDeleted(true);
            cat.setDeletedAt(LocalDateTime.now());
            categoryRepository.save(cat);
            modified.add(Map.of("id", cat.getId(), "name", cat.getName()));
        }

        return ResponseEntity.ok(new ApiResponse("success", "Category soft deleted", modified));
    }

    /** Soft delete categories in bulk */
    @Transactional
    public ResponseEntity<ApiResponse> softDeleteInBulk(List<Long> ids) {
        List<Map<String, Object>> modifiedCategories = new ArrayList<>();

        for (Long id : ids) {
            Category category = categoryRepository.findById(id)
                    .orElseThrow(() -> new CategoryNotFoundException("Category not found: " + id));

            List<Category> allCategories = new ArrayList<>();
            collectCategoryAndChildren(category, allCategories);

            for (Category cat : allCategories) {
                cat.setDeleted(true);
                cat.setDeletedAt(LocalDateTime.now());
                categoryRepository.save(cat);
                modifiedCategories.add(Map.of("id", cat.getId(), "name", cat.getName()));
            }
        }

        return ResponseEntity.ok(new ApiResponse("success", "Categories soft deleted", modifiedCategories));
    }

    /** ====================== HARD DELETE ====================== */

    /** Internal recursive hard delete helper using native SQL */
    @Transactional
    public void hardDeleteCategoryInternalOptimized(Long categoryId, List<Map<String, Object>> deleted) {
        deleted.add(Map.of("id", categoryId));

        // Detach suppliers & products in bulk
        categoryRepository.detachSuppliers(categoryId);
        categoryRepository.detachProducts(categoryId);

        // Recursively delete subcategories
        List<Long> subIds = categoryRepository.findSubcategoryIds(categoryId);
        for (Long subId : subIds) {
            hardDeleteCategoryInternalOptimized(subId, deleted);
        }

        // Delete category itself
        categoryRepository.deleteCategoryById(categoryId);
    }

    /** Single-category hard delete */
    @Transactional
    public ResponseEntity<ApiResponse> hardDelete(Long id) {
        List<Map<String, Object>> deleted = new ArrayList<>();
        hardDeleteCategoryInternalOptimized(id, deleted);

        return ResponseEntity.ok(new ApiResponse(
                "success",
                "Category permanently deleted",
                deleted
        ));
    }

    /** Bulk hard delete */
    @Transactional
    public ResponseEntity<ApiResponse> hardDeleteInBulk(List<Long> ids) {
        List<Map<String, Object>> deletedCategories = new ArrayList<>();
        for (Long id : ids) {
            hardDeleteCategoryInternalOptimized(id, deletedCategories);
        }

        return ResponseEntity.ok(new ApiResponse(
                "success",
                "Categories permanently deleted",
                deletedCategories
        ));
    }


    // --------------------- RESTORE ---------------------

    /** Restore single category (non-recursive) */
    @Transactional
    public ResponseEntity<ApiResponse> restore(Long id) {
        Category category = categoryRepository.findById(id)
                .orElseThrow(() -> new CategoryNotFoundException("Category not found: " + id));

        category.setDeleted(false);
        category.setDeletedAt(null);
        categoryRepository.save(category);

        Map<String, Object> restored = Map.of("id", category.getId(), "name", category.getName());
        return ResponseEntity.ok(new ApiResponse("success", "Category restored", restored));
    }

    /** Restore categories in bulk (non-recursive) */
    @Transactional
    public ResponseEntity<ApiResponse> restoreInBulk(List<Long> ids) {
        List<Map<String, Object>> restoredCategories = new ArrayList<>();

        for (Long id : ids) {
            Category category = categoryRepository.findById(id)
                    .orElseThrow(() -> new CategoryNotFoundException("Category not found: " + id));

            category.setDeleted(false);
            category.setDeletedAt(null);
            categoryRepository.save(category);

            restoredCategories.add(Map.of("id", category.getId(), "name", category.getName()));
        }

        return ResponseEntity.ok(new ApiResponse("success", "Categories restored", restoredCategories));
    }

    // --------------------- RECURSIVE RESTORE ---------------------

    /** Restore category + all subcategories recursively */
    @Transactional
    public ResponseEntity<ApiResponse> restoreRecursively(Long id) {
        Category category = categoryRepository.findById(id)
                .orElseThrow(() -> new CategoryNotFoundException("Category not found: " + id));

        List<Category> allCategories = new ArrayList<>();
        collectCategoryAndChildren(category, allCategories);

        List<Map<String, Object>> restoredCategories = new ArrayList<>();
        for (Category cat : allCategories) {
            cat.setDeleted(false);
            cat.setDeletedAt(null);
            categoryRepository.save(cat);
            restoredCategories.add(Map.of("id", cat.getId(), "name", cat.getName()));
        }

        return ResponseEntity.ok(new ApiResponse("success", "Category and subcategories restored", restoredCategories));
    }

    /** Bulk recursive restore */
    @Transactional
    public ResponseEntity<ApiResponse> restoreCategoriesRecursivelyInBulk(List<Long> ids) {
        List<Map<String, Object>> restoredCategories = new ArrayList<>();

        for (Long id : ids) {
            Category category = categoryRepository.findById(id)
                    .orElseThrow(() -> new CategoryNotFoundException("Category not found: " + id));

            List<Category> allCategories = new ArrayList<>();
            collectCategoryAndChildren(category, allCategories);

            for (Category cat : allCategories) {
                cat.setDeleted(false);
                cat.setDeletedAt(null);
                categoryRepository.save(cat);
                restoredCategories.add(Map.of("id", cat.getId(), "name", cat.getName()));
            }
        }

        return ResponseEntity.ok(new ApiResponse("success", "Categories and subcategories restored", restoredCategories));
    }

    // ---------------- SEARCH ----------------

    /**
     * Search and return a hierarchical tree of matching categories.
     * Active only if activeOnly=true, otherwise all categories.
     */
    public List<CategoryDTO> searchByKeyword(String keyword, boolean deleted) {
        List<Category> matching = deleted == false
                ? categoryRepository.searchActive(keyword)
                : categoryRepository.searchAll(keyword);

        // Convert to DTOs
        List<CategoryDTO> dtos = matching.stream()
                .map(categoryMapper::toDTO)
                .toList();

        // Build tree
        return buildTreeFromList(dtos);
    }

    /**
     * Build hierarchical tree from a flat list of CategoryDTOs.
     */
    private List<CategoryDTO> buildTreeFromList(List<CategoryDTO> categories) {
        Map<Long, List<CategoryDTO>> childrenMap = categories.stream()
                .filter(c -> c.getParentId() != null)
                .collect(Collectors.groupingBy(CategoryDTO::getParentId));

        List<CategoryDTO> roots = categories.stream()
                .filter(c -> c.getParentId() == null || !categories.stream().anyMatch(p -> p.getId().equals(c.getParentId())))
                .toList();

        for (CategoryDTO root : roots) {
            attachChildren(root, childrenMap);
        }

        return roots;
    }

    private void attachChildren(CategoryDTO parent, Map<Long, List<CategoryDTO>> childrenMap) {
        List<CategoryDTO> children = childrenMap.getOrDefault(parent.getId(), new ArrayList<>());
        parent.setSubcategories(children);
        children.forEach(child -> attachChildren(child, childrenMap));
    }

    public List<Long> getAllCategoryIdsRecursive(Long categoryId) {
        Category root = categoryRepository.findById(categoryId)
                .orElseThrow(() -> new CategoryNotFoundException("Category not found: " + categoryId));

        List<Long> ids = new ArrayList<>();
        collectCategoryIds(root, ids);
        return ids;
    }

    private void collectCategoryIds(Category category, List<Long> ids) {
        ids.add(category.getId());
        if (category.getSubcategories() != null) {
            for (Category sub : category.getSubcategories()) {
                collectCategoryIds(sub, ids);
            }
        }
    }

    public void validateAccess() {
        Role role = SecurityUtils.currentRole();

    }
}