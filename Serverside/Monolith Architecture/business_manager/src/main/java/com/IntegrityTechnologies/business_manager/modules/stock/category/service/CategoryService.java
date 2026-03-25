package com.IntegrityTechnologies.business_manager.modules.stock.category.service;

import com.IntegrityTechnologies.business_manager.config.caffeine.CacheInvalidationService;
import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.exception.CategoryNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.control.CloseChecklistService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.repository.SupplierRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.category.dto.CategoryDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.category.mapper.CategoryMapper;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.CategorySupplier;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.CategorySupplierId;
import com.IntegrityTechnologies.business_manager.modules.stock.category.repository.CategoryRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.dto.SupplierDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.mapper.SupplierMapper;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.security.util.BranchContext;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.SecurityUtils;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional
public class CategoryService {

    private final CategoryRepository categoryRepository;
    private final CategoryMapper categoryMapper;
    private final SupplierMapper supplierMapper;
    private final SupplierRepository supplierRepository;
    private final BranchRepository branchRepository;
    private final CacheInvalidationService cacheInvalidationService;
    private final BranchTenantGuard branchTenantGuard;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    // ---------------- SAVE / UPDATE ----------------
    public CategoryDTO saveCategory(CategoryDTO dto) {

        UUID branchId = dto.getBranchId();
        branchTenantGuard.validate(branchId);

        Category category = dto.getId() != null
                ? categoryRepository.findByIdSafe(dto.getId(), null, tenantId(), branchId)
                .orElse(new Category())
                : new Category();

        Long parentId = dto.getParentId();

        boolean exists = categoryRepository
                .existsByNameAndParentSafe(dto.getName(), parentId, tenantId(), branchId);

        if (exists) {
            if (dto.getId() == null) {
                throw new IllegalArgumentException(
                        "Category already exists under this parent: " + dto.getName()
                );
            }

            Category existing = categoryRepository
                    .findByNameSafe(dto.getName(), false, tenantId(), branchId)
                    .orElse(null);

            if (existing != null && !existing.getId().equals(dto.getId())) {
                throw new IllegalArgumentException(
                        "Category already exists under this parent: " + dto.getName()
                );
            }
        }
        validateProfit(dto);

        categoryMapper.updateEntityFromDTO(dto, category);

        if (dto.getParentId() != null) {

            if (dto.getParentId().equals(dto.getId()))
                throw new IllegalArgumentException("Category cannot be its own parent");

            Category parent = categoryRepository.findByIdSafe(
                    dto.getParentId(),
                    false,
                    tenantId(),
                    branchId
            ).orElseThrow(() -> new CategoryNotFoundException("Parent category not found"));

            if (isChildOf(parent, category))
                throw new IllegalArgumentException("Cannot set parent: would create cycle");

            category.setParent(parent);
        } else {
            category.setParent(null);
        }

        /*
         * IMPORTANT FIX:
         * Set temporary path before first save to satisfy NOT NULL constraint
         */
        category.setPath("/TEMP");

        if(dto.getMinimumProfit() != null)
            category.setMinimumProfit(dto.getMinimumProfit());
        if(dto.getMinimumPercentageProfit() != null)
            category.setMinimumPercentageProfit(dto.getMinimumPercentageProfit());

        category = categoryRepository.save(category);

        /*
         * Now generate correct path using ID
         */
        if (category.getParent() == null) {
            category.setPath("/" + category.getId());
        } else {
            category.setPath(category.getParent().getPath() + "/" + category.getId());
        }

        category = categoryRepository.save(category);

        /*
         * Handle suppliers AFTER category ID exists
         */
        if (dto.getSupplierIds() != null) {

            category.getCategorySuppliers().clear();

            for (UUID supplierId : dto.getSupplierIds()) {

                Supplier supplier = supplierRepository.findByIdSafe(
                        supplierId,
                        null,
                        tenantId(),
                        branchId
                ).orElseThrow(() ->
                        new IllegalArgumentException("Supplier not found: " + supplierId)
                );

                CategorySupplier relation = CategorySupplier.builder()
                        .id(new CategorySupplierId(category.getId(), supplierId))
                        .category(category)
                        .supplier(supplier)
                        .build();

                category.getCategorySuppliers().add(relation);
            }

            category = categoryRepository.saveAndFlush(category);
        }

        CategoryDTO result = categoryMapper.toDTO(category);

        result.setSubcategories(
                category.getSubcategories() != null
                        ? category.getSubcategories()
                        .stream()
                        .map(categoryMapper::toDTO)
                        .toList()
                        : List.of()
        );

        cacheInvalidationService.evictCategoryCaches(branchId);
        return result;
    }

    private void validateProfit(CategoryDTO dto) {

        if (dto.getMinimumPercentageProfit() != null) {
            if (dto.getMinimumPercentageProfit() < 0 || dto.getMinimumPercentageProfit() > 100) {
                throw new IllegalArgumentException("Percentage must be 0–100");
            }
        }

        if (dto.getMinimumProfit() != null) {
            if (dto.getMinimumProfit().signum() < 0) {
                throw new IllegalArgumentException("Minimum profit cannot be negative");
            }
        }
    }

    @Transactional
    public Category createMinimal(String name, String branchName) {
        UUID branchId = findBranch(branchName);

        Category category = categoryRepository
                .findByNameSafe(name, false, tenantId(), branchId)
                .orElseGet(() -> {

                    Category newCategory = Category.builder()
                            .name(name)
                            .tenantId(tenantId())
                            .branchId(branchId)
                            .path("/TEMP")
                            .build();

                    newCategory = categoryRepository.save(newCategory);

                    if (newCategory.getParent() == null) {
                        newCategory.setPath("/" + newCategory.getId());
                    } else {
                        newCategory.setPath(
                                newCategory.getParent().getPath() + "/" + newCategory.getId()
                        );
                    }

                    return categoryRepository.save(newCategory);
                });

        cacheInvalidationService.evictCategoryCaches(branchId);
        return category;
    }

    @Cacheable(
            value = "branch-lookup",
            key = "T(java.util.Objects).hash(T(com.IntegrityTechnologies.business_manager.security.util.TenantContext).getTenantId(), #branchName)"
    )
    public UUID findBranch(String branchName) {
        return branchRepository.findByTenantIdAndBranchCodeIgnoreCaseAndDeletedFalse(tenantId(), branchName)
                .orElseThrow(() -> new IllegalArgumentException("Branch not found: " + branchName)).getId();
    }


    @Transactional
    public CategoryDTO updateCategoryRecursive(CategoryDTO dto) {
        UUID branchId = dto.getBranchId();

        Category category = categoryRepository.findByIdSafe(
                dto.getId(),
                null,
                tenantId(),
                branchId
        ).orElseThrow(() -> new CategoryNotFoundException("Category not found"));

        String oldPath = category.getPath();

        Long parentId = dto.getParentId();

        boolean exists = categoryRepository
                .existsByNameAndParentSafe(dto.getName(), parentId, tenantId(), branchId);

        if (exists) {

            Category existing = categoryRepository
                    .findByNameSafe(dto.getName(), false, tenantId(), branchId)
                    .orElse(null);

            if (existing != null && !existing.getId().equals(dto.getId())) {
                throw new IllegalArgumentException(
                        "Category already exists under this parent: " + dto.getName()
                );
            }
        }

        categoryMapper.updateEntityFromDTO(dto, category);

        /*
         * HANDLE PARENT (SAFE)
         */
        if (dto.getParentId() != null) {

            if (dto.getParentId().equals(dto.getId()))
                throw new IllegalArgumentException("Category cannot be its own parent");

            Category parent = categoryRepository.findByIdSafe(
                    dto.getParentId(),
                    false,
                    tenantId(),
                    branchId
            ).orElseThrow(() -> new CategoryNotFoundException("Parent category not found"));

            if (isChildOf(parent, category))
                throw new IllegalArgumentException("Cannot set parent: would create cycle");

            category.setParent(parent);

        } else {
            category.setParent(null);
        }

        category = categoryRepository.save(category);

        /*
         * OPTIMIZED PATH REWRITE (SAFE)
         */
        String newPath;

        if (category.getParent() == null) {
            newPath = "/" + category.getId();
        } else {
            newPath = category.getParent().getPath() + "/" + category.getId();
        }

        if (!oldPath.equals(newPath)) {

            categoryRepository.rewriteSubtreePathSafe(
                    oldPath,
                    newPath,
                    tenantId(),
                    branchId
            );

            category.setPath(newPath);
        }

        category = categoryRepository.save(category);

        /*
         * ✅ SUPPLIER UPDATE (RESTORED + SAFE)
         */
        if (dto.getSupplierIds() != null) {

            category.getCategorySuppliers().clear();

            for (UUID supplierId : dto.getSupplierIds()) {

                Supplier supplier = supplierRepository.findByIdSafe(
                        supplierId,
                        false,
                        tenantId(),
                        branchId
                ).orElseThrow(() ->
                        new IllegalArgumentException("Supplier not found: " + supplierId)
                );

                CategorySupplier relation = CategorySupplier.builder()
                        .id(new CategorySupplierId(category.getId(), supplierId))
                        .category(category)
                        .supplier(supplier)
                        .build();

                category.getCategorySuppliers().add(relation);
            }

            category = categoryRepository.saveAndFlush(category);
        }

        /*
         * ✅ DTO ENRICHMENT (RESTORED)
         */
        CategoryDTO result = categoryMapper.toDTO(category);

        result.setSubcategories(
                category.getSubcategories() != null
                        ? category.getSubcategories()
                        .stream()
                        .map(categoryMapper::toDTO)
                        .toList()
                        : List.of()
        );

        cacheInvalidationService.evictCategoryCaches(branchId);
        return result;
    }

    /**
     * Recursively propagate deleted/restored state to children from the parent.
     */

    private boolean isChildOf(Category potentialParent, Category category) {
        if (potentialParent == null) return false;
        if (potentialParent.getId().equals(category.getId())) return true;
        return isChildOf(potentialParent.getParent(), category);
    }
    public void validateCategoryForBulk(CategoryDTO dto) {

        UUID branchId = dto.getBranchId();
        branchTenantGuard.validate(branchId);

        Long parentId = dto.getParentId();

        boolean exists = categoryRepository
                .existsByNameAndParentSafe(dto.getName(), parentId, tenantId(), branchId);

        if (exists && dto.getId() == null) {
            throw new IllegalArgumentException(
                    "Category already exists under this parent: " + dto.getName()
            );
        }

        validateProfit(dto);
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

        Map<Long, Category> entityMap = categories.stream()
                .collect(Collectors.toMap(Category::getId, c -> c));

        Map<Long, List<Category>> childrenMap = categories.stream()
                .filter(c -> c.getParent() != null)
                .collect(Collectors.groupingBy(c -> c.getParent().getId()));

        List<Category> roots = categories.stream()
                .filter(c -> c.getParent() == null
                        || !entityMap.containsKey(c.getParent().getId()))
                .toList();

        return roots.stream()
                .map(root -> buildTreeRecursive(root, childrenMap))
                .toList();
    }

    public List<CategoryDTO> getAllCategories(UUID branchId, String mode, Boolean deleted) {
        if("tree".equals(mode))
            return getAllCategoriesTree(branchId, deleted);
        else
            return getAllCategoriesFlat(branchId, deleted);
    }

    @Cacheable(
            value = "category-tree",
            key = "T(com.IntegrityTechnologies.business_manager.modules.stock.category.cache.CategoryCacheKey).tree(#branchId, #deleted)"
    )
    public List<CategoryDTO> getAllCategoriesTree(UUID branchId, Boolean deleted) {

        List<Category> ordered = categoryRepository.findAllOrderedSafe(deleted, tenantId(), branchId);

        Map<Long, CategoryDTO> dtoMap = new LinkedHashMap<>();
        List<CategoryDTO> roots = new ArrayList<>();

        for (Category entity : ordered) {

            CategoryDTO dto = categoryMapper.toDTO(entity);
            dto.setSubcategories(new ArrayList<>());

            dtoMap.put(dto.getId(), dto);

            if (entity.getParent() == null) {
                roots.add(dto);
            } else {
                CategoryDTO parentDto = dtoMap.get(entity.getParent().getId());
                if (parentDto != null) {
                    parentDto.getSubcategories().add(dto);
                }
            }
        }

        return roots;
    }

    @Cacheable(
            value = "category-flat",
            key = "T(com.IntegrityTechnologies.business_manager.modules.stock.category.cache.CategoryCacheKey).flat(#branchId, #deleted)"
    )
    public List<CategoryDTO> getAllCategoriesFlat(UUID branchId, Boolean deleted) {

        List<Category> categories = categoryRepository
                .findAllOrderedSafe(deleted, tenantId(), branchId);

        return categories.stream()
                .map(categoryMapper::toDTO)
                .collect(Collectors.toList());
    }

    // ---------------- SINGLE CATEGORY ----------------

    public List<SupplierDTO> getCategorySuppliers(UUID branchId, Long id, Boolean deleted, Boolean strict) {

        Category category = categoryRepository.findByIdSafe(
                id,
                null,
                tenantId(),
                branchId
        ).orElseThrow(() -> new CategoryNotFoundException("Category not found: " + id));

        Set<Supplier> suppliers = new HashSet<>();

        if (Boolean.TRUE.equals(strict)) {

            suppliers.addAll(
                    category.getCategorySuppliers().stream()
                            .map(CategorySupplier::getSupplier)
                            .toList()
            );

        } else {

            Category current = category;
            while (current != null) {

                suppliers.addAll(
                        current.getCategorySuppliers().stream()
                                .map(CategorySupplier::getSupplier)
                                .toList()
                );

                current = current.getParent();
            }
        }

        List<Supplier> filtered = suppliers.stream()
                .filter(s ->
                        deleted == null
                                || deleted.equals(s.getDeleted())
                )
                .toList();

        return supplierMapper.toDTOList(filtered);
    }

    public CategoryDTO getCategory(UUID branchId, Long id, String mode, Boolean deleted) {
        if("tree".equals(mode))
            return getCategoryTree(branchId, id, deleted);
        else
            return getCategoryFlat(branchId, id, deleted);
    }

    public CategoryDTO getCategoryTree(UUID branchId, Long id, Boolean deleted) {

        Category root = categoryRepository.findByIdSafe(
                id,
                deleted,
                tenantId(),
                branchId
        ).orElseThrow(() -> new CategoryNotFoundException("Category not found"));

        if (deleted != null) {
            if (deleted && !root.isDeleted())
                throw new CategoryNotFoundException("Category not found or inactive");
            if (!deleted && root.isDeleted())
                throw new CategoryNotFoundException("Category not found or inactive");
        }

        List<Category> subtree = categoryRepository.findSubtreeSafe(
                root.getPath(),
                tenantId(),
                branchId
        );

        return buildTree(subtree).stream()
                .filter(dto -> dto.getId().equals(id))
                .findFirst()
                .orElseThrow(() -> new CategoryNotFoundException("Category not found"));
    }

    private CategoryDTO buildCategoryTreeFromFlat(Category root, List<Category> all) {

        Map<Long, List<Category>> childrenMap = all.stream()
                .filter(c -> c.getParent() != null)
                .collect(Collectors.groupingBy(c -> c.getParent().getId()));

        return buildTreeRecursive(root, childrenMap);
    }

    private CategoryDTO buildTreeRecursive(Category category,
                                           Map<Long, List<Category>> childrenMap) {

        CategoryDTO dto = categoryMapper.toDTO(category);

        List<Category> children = childrenMap.getOrDefault(category.getId(), List.of());

        dto.setSubcategories(
                children.stream()
                        .map(child -> buildTreeRecursive(child, childrenMap))
                        .toList()
        );

        return dto;
    }

    public CategoryDTO getCategoryFlat(UUID branchId, Long id, Boolean deleted) {
        if(Boolean.FALSE.equals(deleted)) {
            Category category = categoryRepository.findByIdSafe(id, false, tenantId(), branchId)
                    .orElseThrow(() -> new CategoryNotFoundException("Category not found or inactive"));
            CategoryDTO dto = categoryMapper.toDTO(category);
            dto.setSubcategories(null); // flat
            return dto;
        } else if(Boolean.TRUE.equals(deleted)) {
            Category category = categoryRepository.findByIdSafe(id, true, tenantId(), branchId)
                    .orElseThrow(() -> new CategoryNotFoundException("Category not found or active"));
            CategoryDTO dto = categoryMapper.toDTO(category);
            dto.setSubcategories(null);
            return dto;
        } else {
            Category category = categoryRepository.findByIdSafe(id, null, tenantId(), branchId)
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
//    private void collectCategoryAndChildren(Category category, List<Category> collector) {
//        collector.add(category);
//        if (category.getSubcategories() != null) {
//            category.getSubcategories().forEach(sub -> collectCategoryAndChildren(sub, collector));
//        }
//    }

    /** Soft delete a single category */
    @Transactional
    public ResponseEntity<ApiResponse> softDelete(UUID branchId, Long id) {

        Category category = categoryRepository.findByIdSafe(
                id,
                false,
                tenantId(),
                branchId
        ).orElseThrow(() -> new CategoryNotFoundException("Category not found: " + id));

        categoryRepository.softDeleteByPathSafe(
                category.getPath(),
                tenantId(),
                branchId
        );

        cacheInvalidationService.evictCategoryCaches(branchId);
        return ResponseEntity.ok(
                new ApiResponse("success", "Category and subcategories soft deleted")
        );
    }

    /** Soft delete categories in bulk */
    @Transactional
    public ResponseEntity<ApiResponse> softDeleteInBulk(UUID branchId, List<Long> ids) {

        for (Long id : ids) {

            Category category = categoryRepository.findByIdSafe(
                    id,
                    false,
                    tenantId(),
                    branchId
            ).orElseThrow(() -> new CategoryNotFoundException("Category not found: " + id));

            categoryRepository.softDeleteByPathSafe(
                    category.getPath(),
                    tenantId(),
                    branchId
            );
        }

        cacheInvalidationService.evictCategoryCaches(branchId);
        return ResponseEntity.ok(
                new ApiResponse("success", "Categories and subcategories soft deleted")
        );
    }

    /** ====================== HARD DELETE ====================== */

    /** Single-category hard delete */
    @Transactional
    public ResponseEntity<ApiResponse> hardDelete(UUID branchId, Long id) {

        Category category = categoryRepository.findByIdSafe(
                id,
                null,
                tenantId(),
                branchId
        ).orElseThrow(() -> new CategoryNotFoundException("Category not found: " + id));

        categoryRepository.hardDeleteByPathSafe(
                category.getPath(),
                tenantId(),
                branchId
        );

        cacheInvalidationService.evictCategoryCaches(branchId);
        return ResponseEntity.ok(
                new ApiResponse("success", "Category permanently deleted")
        );
    }

    /** Bulk hard delete */
    @Transactional
    public ResponseEntity<ApiResponse> hardDeleteInBulk(UUID branchId, List<Long> ids) {

        for (Long id : ids) {

            Category category = categoryRepository.findByIdSafe(
                    id,
                    null,
                    tenantId(),
                    branchId
            ).orElseThrow(() -> new CategoryNotFoundException("Category not found: " + id));

            categoryRepository.hardDeleteByPathSafe(
                    category.getPath(),
                    tenantId(),
                    branchId
            );
        }

        cacheInvalidationService.evictCategoryCaches(branchId);
        return ResponseEntity.ok(
                new ApiResponse("success", "Categories permanently deleted")
        );
    }


    // --------------------- RESTORE ---------------------

    /** Restore single category (non-recursive) */
    @Transactional
    public ResponseEntity<ApiResponse> restore(UUID branchId, Long id) {

        Category category = categoryRepository.findByIdSafe(
                id,
                null,
                tenantId(),
                branchId
        ).orElseThrow(() -> new CategoryNotFoundException("Category not found: " + id));

        category.setDeleted(false);
        category.setDeletedAt(null);
        categoryRepository.save(category);

        Map<String, Object> restored = Map.of("id", category.getId(), "name", category.getName());

        cacheInvalidationService.evictCategoryCaches(branchId);
        return ResponseEntity.ok(
                new ApiResponse("success", "Category restored", restored)
        );
    }

    /** Restore categories in bulk (non-recursive) */
    @Transactional
    public ResponseEntity<ApiResponse> restoreInBulk(UUID branchId, List<Long> ids) {

        List<Map<String, Object>> restoredCategories = new ArrayList<>();

        for (Long id : ids) {

            Category category = categoryRepository.findByIdSafe(
                    id,
                    null,
                    tenantId(),
                    branchId
            ).orElseThrow(() -> new CategoryNotFoundException("Category not found: " + id));

            category.setDeleted(false);
            category.setDeletedAt(null);
            categoryRepository.save(category);

            restoredCategories.add(
                    Map.of("id", category.getId(), "name", category.getName())
            );
        }

        cacheInvalidationService.evictCategoryCaches(branchId);
        return ResponseEntity.ok(
                new ApiResponse("success", "Categories restored", restoredCategories)
        );
    }

    // --------------------- RECURSIVE RESTORE ---------------------

    /** Restore category + all subcategories recursively */
    @Transactional
    public ResponseEntity<ApiResponse> restoreRecursively(UUID branchId, Long id) {

        Category category = categoryRepository.findByIdSafe(
                id,
                true,
                tenantId(),
                branchId
        ).orElseThrow(() -> new CategoryNotFoundException("Category not found: " + id));

        categoryRepository.restoreByPathSafe(
                category.getPath(),
                tenantId(),
                branchId
        );

        cacheInvalidationService.evictCategoryCaches(branchId);
        return ResponseEntity.ok(
                new ApiResponse("success", "Category and subcategories restored")
        );
    }

    /** Bulk recursive restore */
    @Transactional
    public ResponseEntity<ApiResponse> restoreCategoriesRecursivelyInBulk(UUID branchId, List<Long> ids) {

        for (Long id : ids) {

            Category category = categoryRepository.findByIdSafe(
                    id,
                    true,
                    tenantId(),
                    branchId
            ).orElseThrow(() ->
                    new CategoryNotFoundException("Category not found: " + id)
            );

            categoryRepository.restoreByPathSafe(
                    category.getPath(),
                    tenantId(),
                    branchId
            );
        }

        cacheInvalidationService.evictCategoryCaches(branchId);
        return ResponseEntity.ok(
                new ApiResponse("success", "Categories and subcategories restored")
        );
    }

    // ---------------- SEARCH ----------------

    /**
     * Search and return a hierarchical tree of matching categories.
     * Active only if activeOnly=true, otherwise all categories.
     */
    @Cacheable(
            value = "category-search",
            key = "T(com.IntegrityTechnologies.business_manager.modules.stock.category.cache.CategoryCacheKey).search(#branchId, #keyword, #deleted)"
    )
    public List<CategoryDTO> searchByKeyword(UUID branchId, String keyword, boolean deleted) {

        List<Category> matching = categoryRepository.searchSafe(
                keyword,
                deleted,
                tenantId(),
                branchId
        );

        List<CategoryDTO> dtos = matching.stream()
                .map(categoryMapper::toDTO)
                .toList();

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

    public List<Long> getAllCategoryIdsRecursive(UUID branchId, Long categoryId) {

        Category root = categoryRepository.findByIdSafe(
                categoryId,
                null,
                tenantId(),
                branchId
        ).orElseThrow(() -> new CategoryNotFoundException("Category not found: " + categoryId));

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
}