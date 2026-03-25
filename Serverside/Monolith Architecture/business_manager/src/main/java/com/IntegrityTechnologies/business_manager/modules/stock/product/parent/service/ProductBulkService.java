package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service;

import com.IntegrityTechnologies.business_manager.config.bulk.BulkOptions;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkResult;
import com.IntegrityTechnologies.business_manager.config.util.PhoneAndEmailNormalizer;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.repository.SupplierRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.service.SupplierService;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.stock.category.repository.CategoryRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.category.service.CategoryService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.ProductVariantCreateDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.ProductVariantDTO;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.*;

@Service
@RequiredArgsConstructor
public class ProductBulkService {

    private final ProductService productService;
    private final CategoryRepository categoryRepository;
    private final SupplierRepository supplierRepository;
    private final SupplierService supplierService;
    private final CategoryService categoryService;
    private final BranchRepository branchRepository;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    private String normalize(String name) {
        return PhoneAndEmailNormalizer.toTitleCase(name);
    }

    @Transactional
    public BulkResult<ProductDTO> bulkFullCreateFrontend(
            ProductBulkFrontendRequestDTO request,
            List<MultipartFile> files
    ) throws IOException {

        BulkResult<ProductDTO> result = new BulkResult<>();

        if (request == null || request.getProducts() == null) {
            return result;
        }

        BulkOptions options =
                request.getOptions() != null
                        ? request.getOptions()
                        : new BulkOptions();

        result.setTotal(request.getProducts().size());

    /* ================================
       DRY RUN MODE (UNCHANGED LOGIC)
       ================================ */

        if (options.isDryRun()) {

            for (int i = 0; i < request.getProducts().size(); i++) {

                int rowNumber = i + 1;
                ProductBulkFrontendRowDTO row = request.getProducts().get(i);

                UUID branchId = findBranch(row.getBranchName());

                try {

                    validateFrontendRow(row);
                    validateCategoryAndSuppliers(row, options);

                    result.addSuccess(
                            ProductDTO.builder()
                                    .name(normalize(row.getName()))
                                    .description(row.getDescription())
                                    .branchId(branchId)
                                    .categoryName(row.getCategoryName())
                                    .minimumPercentageProfit(row.getMinimumPercentageProfit())
                                    .variants(
                                            row.getVariants() == null
                                                    ? List.of()
                                                    : row.getVariants().stream()
                                                    .map(v -> ProductVariantDTO.builder()
                                                            .classification(v)
                                                            .build())
                                                    .toList()
                                    )
                                    .build()
                    );

                } catch (Exception ex) {
                    result.addError(rowNumber, ex.getMessage());
                }
            }

            return result;
        }

    /* ================================
       REAL INSERT (ALL OR NOTHING)
       ================================ */

        List<ProductDTO> created = new ArrayList<>();

        for (int i = 0; i < request.getProducts().size(); i++) {

            ProductBulkFrontendRowDTO row = request.getProducts().get(i);
            UUID branchId = findBranch(row.getBranchName());

            ProductFullCreateDTO internal =
                    mapToInternalDTO(row, i, request);

            List<MultipartFile> relevantFiles =
                    filterMultipartFilesForRow(i, request, files);

            ProductDTO saved =
                    productService.fullCreate(branchId, internal, relevantFiles);

            created.add(saved);
        }

        result.setSuccess(created.size());
        result.setFailed(0);
        result.setData(created);

        return result;
    }

    private ProductFullCreateDTO mapToInternalDTO(
            ProductBulkFrontendRowDTO row,
            int rowIndex,
            ProductBulkFrontendRequestDTO request
    ) {
        UUID branchId = findBranch(row.getBranchName());

        String normalizedName = normalize(row.getName());

        Category category = categoryRepository
                .findByNameSafe(row.getCategoryName(), false, tenantId(), branchId)
                .orElseGet(() -> {

                    if (!request.getOptions().isCreateMissingCategories()) {
                        throw new IllegalArgumentException(
                                "Unknown category: " + row.getCategoryName()
                        );
                    }

                    return categoryService.createMinimal(row.getCategoryName(), row.getBranchName());
                });

        List<UUID> supplierIds = new ArrayList<>();

        if (row.getSupplierNames() != null) {

            for (String supplierName : row.getSupplierNames()) {

                Supplier supplier = supplierRepository
                        .findByNameSafe(supplierName, false, tenantId(), branchId)
                        .orElseGet(() -> {

                            if (!request.getOptions().isCreateMissingSuppliers()) {
                                throw new IllegalArgumentException(
                                        "Unknown supplier: " + supplierName
                                );
                            }

                            return supplierService.createMinimalSupplier(
                                    supplierName,
                                    category
                            );
                        });

                supplierIds.add(supplier.getId());
            }
        }

        ProductCreateDTO product = ProductCreateDTO.builder()
                .name(normalizedName) // 🔥 FIX
                .description(row.getDescription())
                .categoryId(category.getId())
                .supplierIds(supplierIds.isEmpty() ? null : supplierIds)
                .minimumPercentageProfit(row.getMinimumPercentageProfit())
                .build();

        List<ProductVariantCreateDTO> variants =
                (row.getVariants() == null || row.getVariants().isEmpty())
                        ? List.of(defaultVariant())
                        : row.getVariants().stream()
                        .map(v -> {
                            ProductVariantCreateDTO dto = new ProductVariantCreateDTO();
                            dto.setClassification(v);
                            return dto;
                        })
                        .toList();

        List<FileAssignmentDTO> assignments =
                buildFileAssignmentsForRow(rowIndex, request);

        ProductFullCreateDTO full = new ProductFullCreateDTO();
        full.setProduct(product);
        full.setVariants(variants);
        full.setFileAssignments(assignments);

        return full;
    }

    private List<FileAssignmentDTO> buildFileAssignmentsForRow(
            int rowIndex,
            ProductBulkFrontendRequestDTO request
    ) {

        if (request.getFileMetadata() == null
                || request.getFileMetadata().isEmpty()) {
            return List.of();
        }

        List<FileAssignmentDTO> assignments = new ArrayList<>();

        for (ProductBulkFileMetadataDTO meta : request.getFileMetadata()) {

            if (meta.getAssignedRowIndexes() == null
                    || !meta.getAssignedRowIndexes().contains(rowIndex)) {
                continue;
            }

            FileAssignmentDTO dto = new FileAssignmentDTO();
            dto.setFileName(meta.getFileName());
            dto.setAssignToProduct(Boolean.TRUE.equals(meta.getAssignToEntity()));

            if (meta.getRowVariantMap() != null) {

                List<String> variants =
                        meta.getRowVariantMap().get(rowIndex);

                dto.setVariantClassifications(
                        variants == null ? List.of() : variants
                );
            }

            assignments.add(dto);
        }

        return assignments;
    }

    private List<MultipartFile> filterMultipartFilesForRow(
            int rowIndex,
            ProductBulkFrontendRequestDTO request,
            List<MultipartFile> files
    ) {

        if (files == null || files.isEmpty()
                || request.getFileMetadata() == null) {
            return List.of();
        }

        Set<String> fileNamesForRow = new HashSet<>();

        for (ProductBulkFileMetadataDTO meta : request.getFileMetadata()) {

            if (meta.getAssignedRowIndexes() != null
                    && meta.getAssignedRowIndexes().contains(rowIndex)) {

                fileNamesForRow.add(meta.getFileName().trim());
            }
        }

        return files.stream()
                .filter(f -> fileNamesForRow.contains(f.getOriginalFilename()))
                .toList();
    }

    private ProductVariantCreateDTO defaultVariant() {
        ProductVariantCreateDTO dto = new ProductVariantCreateDTO();
        dto.setClassification("STANDARD");
        return dto;
    }

    private void validateCategoryAndSuppliers(
            ProductBulkFrontendRowDTO row,
            BulkOptions options
    ) {
        UUID branchId = findBranch(row.getBranchName());

        if (!categoryRepository
                .existsByNameSafe(row.getCategoryName(), tenantId(), branchId)
                && !options.isCreateMissingCategories()) {

            throw new IllegalArgumentException(
                    "Unknown category: " + row.getCategoryName()
            );
        }

        if (row.getSupplierNames() != null) {
            for (String supplierName : row.getSupplierNames()) {

                if (!supplierRepository
                        .existsByNameSafe(supplierName, tenantId(), branchId)
                        && !options.isCreateMissingSuppliers()) {

                    throw new IllegalArgumentException(
                            "Unknown supplier: " + supplierName
                    );
                }
            }
        }
    }

    private void validateFrontendRow(
            ProductBulkFrontendRowDTO row
    ) {

        if (row.getName() == null || row.getName().isBlank()) {
            throw new IllegalArgumentException("Product name is required");
        }

        if (row.getCategoryName() == null
                || row.getCategoryName().isBlank()) {
            throw new IllegalArgumentException("Category is required");
        }

        if (productService.existsByName(normalize(row.getName()))) {
            throw new IllegalArgumentException(
                    "Product already exists: " + row.getName()
            );
        }

        if (row.getCategoryName() == null
                || row.getCategoryName().isBlank()) {
            throw new IllegalArgumentException("Category is required");
        }

        if (row.getBranchName() == null
                || row.getBranchName().isBlank()) {
            throw new IllegalArgumentException("Branch is required");
        }
    }

    @Cacheable(
            value = "branch-lookup",
            key = "T(java.util.Objects).hash(T(com.IntegrityTechnologies.business_manager.security.util.TenantContext).getTenantId(), #branchName)"
    )
    public UUID findBranch(String branchName) {
        return branchRepository.findByTenantIdAndBranchCodeIgnoreCaseAndDeletedFalse(tenantId(), branchName)
                .orElseThrow(() -> new IllegalArgumentException("Branch not found: " + branchName)).getId();
    }
}