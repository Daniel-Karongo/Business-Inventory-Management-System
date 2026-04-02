package com.IntegrityTechnologies.business_manager.modules.stock.category.service;

import com.IntegrityTechnologies.business_manager.config.bulk.BulkRequest;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkResult;
import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.person.supplier.repository.SupplierRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.category.dto.CategoryBulkRow;
import com.IntegrityTechnologies.business_manager.modules.stock.category.dto.CategoryDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.category.mapper.CategoryMapper;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.CategorySupplier;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.CategorySupplierId;
import com.IntegrityTechnologies.business_manager.modules.stock.category.repository.CategoryRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class CategoryBulkService {

    private final CategoryService categoryService;
    private final CategoryRepository categoryRepository;
    private final SupplierRepository supplierRepository;
    private final CategoryMapper categoryMapper;
    private final BranchTenantGuard branchTenantGuard;
    private final BranchRepository branchRepository;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    @Transactional
    public BulkResult<CategoryDTO> importCategories(BulkRequest<CategoryBulkRow> request) {

        BulkResult<CategoryDTO> result = new BulkResult<>();
        result.setTotal(request.getItems().size());

        boolean dryRun = request.getOptions() != null && request.getOptions().isDryRun();

        Map<String, Category> created = new HashMap<>();

        /*
         * =========================
         * PASS 1 — VALIDATE + PREPARE
         * =========================
         */
        for (int i = 0; i < request.getItems().size(); i++) {

            int rowNum = i + 1;
            CategoryBulkRow row = request.getItems().get(i);
            UUID branchId = row.getBranchId();
            branchTenantGuard.validate(branchId);

            try {
                if (row.getName() == null || row.getName().isBlank()) {
                    throw new IllegalArgumentException("Category name is required");
                }

                // Build DTO (no DB yet)
                CategoryDTO dto = new CategoryDTO();
                dto.setName(row.getName());
                dto.setBranchId(branchId);
                dto.setDescription(row.getDescription());
                dto.setMinimumProfit(row.getMinimumProfit());
                dto.setMinimumPercentageProfit(row.getMinimumPercentageProfit());

                // ALWAYS validate (even in dry-run)
                categoryService.validateCategoryForBulk(dto);

                if (!dryRun) {
                    CategoryDTO saved = categoryService.saveCategory(dto);

                    Category entity = categoryRepository.getReferenceById(saved.getId());

                    created.put(key(row.getName(), branchId), entity);

                } else {
                    Category simulated = new Category();
                    simulated.setName(row.getName());
                    simulated.setMinimumProfit(row.getMinimumProfit());
                    simulated.setMinimumPercentageProfit(row.getMinimumPercentageProfit());

                    created.put(key(row.getName(), branchId), simulated);
                }

            } catch (Exception ex) {
                result.addError(rowNum, ex.getMessage());
            }
        }

        /*
         * =========================
         * PASS 2 — RELATIONSHIPS
         * =========================
         */
        for (int i = 0; i < request.getItems().size(); i++) {

            int rowNum = i + 1;
            CategoryBulkRow row = request.getItems().get(i);
            UUID branchId = row.getBranchId();
            branchTenantGuard.validate(branchId);

            try {
                Category category = created.get(key(row.getName(), branchId));

                if (category == null) continue;

                /*
                 * --- Parent resolution ---
                 */
                if (row.getParentName() != null && !row.getParentName().isBlank()) {

                    Category parent =
                            created.getOrDefault(
                                    key(row.getParentName(), branchId),
                                    categoryRepository.findByNameSafe(
                                            row.getParentName(),
                                            false,
                                            tenantId(),
                                            branchId
                                    ).orElseThrow(() ->
                                            new IllegalArgumentException("Unknown parent: " + row.getParentName())
                                    )
                            );

                    validateNoCycle(row.getName(), parent, created, branchId);

                    // ✅ ALWAYS validate (even in dry-run)
                    if (parent == null) {
                        throw new IllegalArgumentException("Parent not found: " + row.getParentName());
                    }

                    if (!dryRun) {
                        category.setParent(parent);
                    }
                }

                /*
                 * --- Supplier resolution ---
                 */
                if (row.getSupplierNames() != null && !row.getSupplierNames().isEmpty()) {

                    if (!dryRun) {
                        category.getCategorySuppliers().clear();
                    }

                    for (String name : row.getSupplierNames()) {

                        Supplier supplier = supplierRepository
                                .findByNameSafe(name, false, tenantId(), branchId)
                                .orElseThrow(() ->
                                        new IllegalArgumentException("Unknown supplier: " + name)
                                );

                        if (!dryRun) {
                            category.getCategorySuppliers().add(
                                    CategorySupplier.builder()
                                            .id(new CategorySupplierId(category.getId(), supplier.getId()))
                                            .category(category)
                                            .supplier(supplier)
                                            .build()
                            );
                        }
                    }
                }

                /*
                 * --- Persist ONLY if not dry-run ---
                 */
                if (!dryRun) {
                    categoryRepository.saveAndFlush(category);
                    result.addSuccess(categoryMapper.toDTO(category));
                } else {
                    Category parent = null;

                    if (row.getParentName() != null) {
                        parent = created.get(key(row.getParentName(), branchId));
                    }

                    // Dry-run: return simulated DTO
                    CategoryDTO preview = new CategoryDTO();
                    preview.setName(row.getName());
                    preview.setDescription(row.getDescription());
                    preview.setBranchId(branchId);
                    if (parent != null) {
                        preview.setParentId(parent.getId());
                    }
                    result.addSuccess(preview);
                }

            } catch (Exception ex) {
                result.addError(rowNum, ex.getMessage());
            }
        }

        return result;
    }

    private String key(String name, UUID branchId) {
        return branchId + "::" + name.toLowerCase();
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