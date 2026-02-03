package com.IntegrityTechnologies.business_manager.modules.stock.category.service;

import com.IntegrityTechnologies.business_manager.common.bulk.*;
import com.IntegrityTechnologies.business_manager.modules.stock.category.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.stock.category.repository.CategoryRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.repository.SupplierRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class CategoryBulkService {

    private final CategoryService categoryService;
    private final CategoryRepository categoryRepository;
    private final SupplierRepository supplierRepository;

    public BulkResult<CategoryDTO> importCategories(
            BulkRequest<CategoryBulkRow> request
    ) {

        BulkResult<CategoryDTO> result = new BulkResult<>();
        result.setTotal(request.getItems().size());

        Map<String, CategoryBulkRow> rowsByName = new HashMap<>();

        /* =========================
           PASS 1 — VALIDATE & CACHE
           ========================= */
        for (CategoryBulkRow row : request.getItems()) {
            if (row.getName() == null || row.getName().isBlank()) {
                throw new IllegalArgumentException("Category name is required");
            }
            rowsByName.put(row.getName().toLowerCase(), row);
        }

        /* =========================
           PASS 2 — CREATE
           ========================= */
        for (int i = 0; i < request.getItems().size(); i++) {
            int rowNum = i + 1;
            CategoryBulkRow row = request.getItems().get(i);

            try {
                CategoryDTO dto = new CategoryDTO();
                dto.setName(row.getName());
                dto.setDescription(row.getDescription());

                // --- Resolve parent ---
                if (row.getParentName() != null && !row.getParentName().isBlank()) {
                    if (row.getParentName().equalsIgnoreCase(row.getName())) {
                        throw new IllegalArgumentException("Category cannot be its own parent");
                    }

                    Category parent = categoryRepository
                            .findByNameIgnoreCase(row.getParentName())
                            .orElseThrow(() ->
                                    new IllegalArgumentException(
                                            "Unknown parent category: " + row.getParentName()
                                    )
                            );

                    dto.setParentId(parent.getId());
                }

                // --- Resolve suppliers ---
                if (row.getSupplierNames() != null && !row.getSupplierNames().isEmpty()) {
                    Set<UUID> supplierIds = row.getSupplierNames().stream()
                            .map(name ->
                                    supplierRepository.findByNameIgnoreCase(name)
                                            .orElseThrow(() ->
                                                    new IllegalArgumentException(
                                                            "Unknown supplier: " + name
                                                    )
                                            ).getId()
                            )
                            .collect(Collectors.toSet());

                    dto.setSuppliersIds(supplierIds);
                }

                if (!request.getOptions().isDryRun()) {
                    CategoryDTO saved = categoryService.saveCategory(dto);
                    result.addSuccess(saved);
                }

            } catch (Exception ex) {
                result.addError(rowNum, ex.getMessage());
            }
        }

        return result;
    }
}