package com.IntegrityTechnologies.business_manager.modules.stock.category.service;

import com.IntegrityTechnologies.business_manager.config.bulk.BulkRequest;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkResult;
import com.IntegrityTechnologies.business_manager.config.caffeine.CacheInvalidationService;
import com.IntegrityTechnologies.business_manager.modules.stock.category.dto.CategoryBulkRow;
import com.IntegrityTechnologies.business_manager.modules.stock.category.dto.CategoryDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.stock.category.repository.CategoryRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class CategoryBulkService {

    private final CategoryService categoryService;
    private final CategoryRepository categoryRepository;
    private final BranchTenantGuard branchTenantGuard;
    private final CacheInvalidationService cacheInvalidationService;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    @Transactional
    public BulkResult<CategoryDTO> importCategories(BulkRequest<CategoryBulkRow> request) {

        BulkResult<CategoryDTO> result = new BulkResult<>();
        result.setTotal(request.getItems().size());

        boolean dryRun = request.getOptions() != null && request.getOptions().isDryRun();

        Map<String, Category> prepared = new HashMap<>();
        Map<String, CategoryBulkRow> rowMap = new HashMap<>();

    /* =====================================================
       PASS 1 — VALIDATION ONLY (NO DB WRITES)
    ===================================================== */
        for (int i = 0; i < request.getItems().size(); i++) {
            int rowNum = i + 1;
            CategoryBulkRow row = request.getItems().get(i);

            try {
                UUID branchId = row.getBranchId();
                branchTenantGuard.validate(branchId);

                if (row.getName() == null || row.getName().isBlank()) {
                    throw new IllegalArgumentException("Category name is required");
                }

                String key = key(row.getName(), branchId);

                if (prepared.containsKey(key)) {
                    throw new IllegalArgumentException("Duplicate category in file: " + row.getName());
                }

                CategoryDTO dto = new CategoryDTO();
                dto.setName(row.getName());
                dto.setBranchId(branchId);
                dto.setDescription(row.getDescription());
                dto.setMinimumProfit(row.getMinimumProfit());
                dto.setMinimumPercentageProfit(row.getMinimumPercentageProfit());

                categoryService.validateCategoryForBulk(dto);

                Category temp = new Category();
                temp.setName(row.getName());
                temp.setBranchId(branchId);

                prepared.put(key, temp);
                rowMap.put(key, row);

            } catch (Exception ex) {
                result.addError(rowNum, ex.getMessage());
            }
        }

    /* =====================================================
       PASS 2 — RESOLVE PARENTS (NO DB WRITES)
    ===================================================== */
        for (int i = 0; i < request.getItems().size(); i++) {
            int rowNum = i + 1;
            CategoryBulkRow row = request.getItems().get(i);

            try {
                UUID branchId = row.getBranchId();
                String key = key(row.getName(), branchId);

                Category category = prepared.get(key);
                if (category == null) continue;

                /* ================= RESOLVE PARENT FIRST ================= */
                Category parent = null;

                if (row.getParentName() != null && !row.getParentName().isBlank()) {

                    String parentKey = key(normalize(row.getParentName()), branchId);

                    Category parentFromFile = prepared.get(parentKey);

                    if (parentFromFile != null) {
                        parent = parentFromFile;
                    } else {
                        parent = categoryRepository.findByNameSafe(
                                row.getParentName(),
                                false,
                                tenantId(),
                                branchId
                        ).orElse(null);
                    }

                    if (parent == null) {
                        throw new IllegalArgumentException("Unknown parent: " + row.getParentName());
                    }

                    validateNoCycle(row.getName(), parent, prepared, branchId);
                }

                category.setParent(parent);

                /* ================= NOW DO DB DUP CHECK ================= */
                Long parentId = parent != null ? parent.getId() : null;

                boolean exists = categoryRepository.existsByNameAndParentSafe(
                        row.getName(),
                        parentId,
                        tenantId(),
                        branchId
                );

                if (exists) {
                    throw new IllegalArgumentException(
                            "Category already exists under this parent: " + row.getName()
                    );
                }

            } catch (Exception ex) {
                result.addError(rowNum, ex.getMessage());
            }
        }

    /* =====================================================
       ❌ IF ANY ERRORS → ABORT (NO DB WRITES)
    ===================================================== */
        if (!result.getErrors().isEmpty()) {
            return result;
        }

    /* =====================================================
       PASS 3 — PERSIST (ALL OR NOTHING)
    ===================================================== */
        if (!dryRun) {

            Map<String, CategoryDTO> savedMap = new HashMap<>();

            for (CategoryBulkRow row : request.getItems()) {
                saveRecursively(row, prepared, savedMap, result, rowMap);
            }

        } else {

            for (CategoryBulkRow row : request.getItems()) {
                CategoryDTO preview = new CategoryDTO();
                preview.setName(row.getName());
                preview.setBranchId(row.getBranchId());
                result.addSuccess(preview);
            }
        }

        if (!dryRun) {
            Set<UUID> branches = request.getItems().stream()
                    .map(CategoryBulkRow::getBranchId)
                    .collect(Collectors.toSet());

            for (UUID branchId : branches) {
                cacheInvalidationService.evictCategoryCaches(tenantId(), branchId);
            }
        }

        return result;
    }

    private CategoryDTO saveRecursively(
            CategoryBulkRow row,
            Map<String, Category> prepared,
            Map<String, CategoryDTO> savedMap,
            BulkResult<CategoryDTO> result,
            Map<String, CategoryBulkRow> rowMap
    ) {

        String key = key(row.getName(), row.getBranchId());

        if (savedMap.containsKey(key)) {
            return savedMap.get(key);
        }

        Category preparedCategory = prepared.get(key);

        Long parentId = null;

        if (preparedCategory.getParent() != null) {

            Category parent = preparedCategory.getParent();
            String parentKey = key(parent.getName(), row.getBranchId());

            CategoryDTO savedParent;

            if (savedMap.containsKey(parentKey)) {
                savedParent = savedMap.get(parentKey);
            } else {

                CategoryBulkRow parentRow = rowMap.get(parentKey);

                if (parentRow == null) {
                    throw new IllegalStateException("Parent row missing for: " + parent.getName());
                }

                savedParent = saveRecursively(
                        parentRow,
                        prepared,
                        savedMap,
                        result,
                        rowMap
                );
            }

            parentId = savedParent.getId();
        }

        CategoryDTO dto = new CategoryDTO();
        dto.setName(row.getName());
        dto.setBranchId(row.getBranchId());
        dto.setParentId(parentId);

        CategoryDTO saved = categoryService.saveCategory(dto);

        savedMap.put(key, saved);
        result.addSuccess(saved);

        return saved;
    }

    private String key(String name, UUID branchId) {
        return branchId + "::" + normalize(name);
    }

    private String normalize(String value) {
        return value == null
                ? null
                : value.trim().replaceAll("\\s+", " ").toLowerCase();
    }

    private void validateNoCycle(
            String childName,
            Category parent,
            Map<String, Category> created,
            UUID branchId
    ) {
        String child = childName.toLowerCase();

        Category current = parent;

        while (current != null) {

            // 🔴 Detect cycle
            if (current.getName().equalsIgnoreCase(child)) {
                throw new IllegalArgumentException("Cycle detected involving: " + childName);
            }

            /*
             * 🔁 Move up hierarchy:
             * 1. First check created map (CSV hierarchy)
             * 2. Otherwise fallback to DB parent
             */

            Category next = created.get(key(current.getName(), branchId));

            if (next != null && next.getParent() != null) {
                current = next.getParent();
            } else {
                current = current.getParent(); // DB chain
            }
        }
    }
}