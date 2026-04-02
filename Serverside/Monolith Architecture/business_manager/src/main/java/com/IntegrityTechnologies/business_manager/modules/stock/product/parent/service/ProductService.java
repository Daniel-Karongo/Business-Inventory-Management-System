package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service;

import com.IntegrityTechnologies.business_manager.config.files.FileStorageService;
import com.IntegrityTechnologies.business_manager.config.files.TransactionalFileManager;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.exception.ProductNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.repository.SaleRepository;
import com.IntegrityTechnologies.business_manager.modules.person.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.person.supplier.repository.SupplierRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.CategorySupplier;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.CategorySupplierId;
import com.IntegrityTechnologies.business_manager.modules.stock.category.repository.CategoryRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.category.service.CategoryService;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.ProductRestoreOptions;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryItem;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.StockTransaction;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryItemRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.StockTransactionRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.mapper.ProductMapper;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductImageAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductImageRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.ProductVariantCreateDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.ProductVariantDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariantAudit;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariantImage;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariantImageAudit;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.repository.ProductVariantAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.repository.ProductVariantImageAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.repository.ProductVariantImageRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.repository.ProductVariantRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.service.ProductVariantService;
import com.IntegrityTechnologies.business_manager.security.util.BranchContext;
import com.IntegrityTechnologies.business_manager.security.util.SecurityUtils;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

@Service
@RequiredArgsConstructor
@Slf4j
public class ProductService {
    private final ProductSkuService skuService;
    private final ProductRepository productRepository;
    private final ProductImageRepository productImageRepository;
    private final ProductMapper productMapper;
    private final FileStorageService fileStorageService;
    private final TransactionalFileManager transactionalFileManager;
    private final ProductAuditRepository productAuditRepository;
    private final ProductImageAuditRepository productImageAuditRepository;
    private final CategoryRepository categoryRepository;
    private final SupplierRepository supplierRepository;
    private final CategoryService categoryService;
    private final ProductVariantService productVariantService;
    private final ProductVariantRepository productVariantRepository;
    private final ProductImageService productImageService;
    private final InventoryItemRepository inventoryItemRepository;
    private final SaleRepository saleRepository;
    private final StockTransactionRepository stockTransactionRepository;
    private final ProductVariantAuditRepository productVariantAuditRepository;
    private final ProductVariantImageRepository productVariantImageRepository;
    private final ProductVariantImageAuditRepository productVariantImageAuditRepository;

    @PersistenceContext
    private EntityManager em;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }
    

    /* =============================
       READ METHODS (unchanged)
       ============================= */

    @Transactional(readOnly = true)
    public Product getById(UUID branchId, UUID id) {
        return productRepository.findByIdAndTenantIdAndBranchId(id, tenantId(), branchId)
                .orElseThrow(() -> new EntityNotFoundException("Product not found"));
    }

    public List<ProductDTO> getAllProducts(UUID branchId, Boolean deleted) {

        return productRepository
                .findAllWithRelationsByTenantIdAndBranchId(tenantId(), branchId)
                .stream()
                .filter(product -> matchesDeleted(deleted, product.getDeleted()))
                .map(productMapper::toDTO)
                .toList();
    }


    private static final Set<String> ALLOWED_SORT_FIELDS = Set.of(
            "id",
            "name",
            "sku",
            "createdAt",
            "updatedAt",      // ✅ ADD
            "category.name",
            "variantCount",
            "deleted"
    );

    public Page<ProductDTO> getProductsAdvanced(
            UUID branchId,
            List<Long> categoryIds,
            String name,
            String description,
            String keyword,
            BigDecimal minPrice,
            BigDecimal maxPrice,
            Boolean deleted,
            int page,
            int size,
            String sortBy,
            String direction,
            boolean includeDeleted,
            Integer minSuppliers,
            Integer maxSuppliers,
            UUID supplierId) {

        if (sortBy == null || sortBy.isBlank()) {
            sortBy = "id";
        }

        Pageable pageable = PageRequest.of(page, size);

        Specification<Product> spec =
                ProductSpecification.filterProducts(
                        tenantId(),
                        branchId,
                        categoryIds,
                        name,
                        description,
                        keyword,
                        minPrice,
                        maxPrice,
                        deleted,
                        includeDeleted,
                        sortBy,
                        direction,
                        minSuppliers,
                        maxSuppliers,
                        supplierId
                );

        return productRepository
                .findAll(spec, pageable)
                .map(productMapper::toDTO);
    }

    public ProductDTO getProductById(UUID branchId, UUID id, Boolean deleted) {

        Product product = productRepository
                .findByIdAndTenantIdAndBranchId(id, tenantId(), branchId)
                .orElseThrow(() -> new ProductNotFoundException("Product not found: " + id));

        if (deleted != null) {
            if (deleted && !Boolean.TRUE.equals(product.getDeleted())) {
                throw new ProductNotFoundException("Product not found: " + id);
            }
            if (!deleted && Boolean.TRUE.equals(product.getDeleted())) {
                throw new ProductNotFoundException("Product not found: " + id);
            }
        }

        return productMapper.toDTO(product);
    }

    public ProductDTO getProductBySKU(UUID branchId, String sku, Boolean deleted) {

        Product product = productRepository
                .findByTenantIdAndBranchIdAndSku(tenantId(), branchId, sku)
                .orElseThrow(() ->
                        new ProductNotFoundException("Product with SKU not found: " + sku)
                );

        if (deleted != null) {
            if (deleted && !Boolean.TRUE.equals(product.getDeleted())) {
                throw new ProductNotFoundException("Product not found");
            }
            if (!deleted && Boolean.TRUE.equals(product.getDeleted())) {
                throw new ProductNotFoundException("Product not found");
            }
        }

        return productMapper.toDTO(product);
    }

    public List<ProductDTO> getProductsBySupplier(UUID branchId, UUID supplierId, Boolean deleted) {

        List<Product> products =
                productRepository.findAllBySupplierIdAndTenantIdAndBranchId(
                        supplierId,
                        tenantId(),
                        branchId
                );

        if (products.isEmpty()) {
            throw new ProductNotFoundException("No products found for supplier: " + supplierId);
        }

        return products.stream()
                .filter(product -> matchesDeleted(deleted, product.getDeleted()))
                .map(productMapper::toDTO)
                .toList();
    }

    public List<ProductDTO> getProductsByCategory(UUID branchId, Long categoryId, Boolean deleted, Boolean strict) {

        List<Product> products;

        if (Boolean.TRUE.equals(strict)) {

            products = productRepository.findAllByTenantIdAndBranchIdAndCategory_Id(
                    tenantId(),
                    branchId,
                    categoryId
            );

        } else {

            List<Long> categoryIds =
                    categoryService.getAllCategoryIdsRecursive(branchId, categoryId);

            products = productRepository.findAllByTenantIdAndBranchIdAndCategory_IdIn(
                    tenantId(),
                    branchId,
                    categoryIds
            );
        }

        if (products.isEmpty()) {
            throw new ProductNotFoundException("No products found for category: " + categoryId);
        }

        return products.stream()
                .filter(product -> matchesDeleted(deleted, product.getDeleted()))
                .map(productMapper::toDTO)
                .toList();
    }

    /* =============================
       CREATE / UPDATE (with audits & SKU gen)
       ============================= */

    @Transactional
    public ProductDTO fullCreate(
            UUID branchId,
            ProductFullCreateDTO dto,
            List<MultipartFile> files
    ) throws IOException {

    /* =============================
       1️⃣ CREATE PRODUCT
       ============================= */

        ProductDTO productDTO = createProductCore(branchId, dto.getProduct());

        Product product = productRepository.findById(productDTO.getId())
                .orElseThrow(() -> new EntityNotFoundException("Product not found"));

    /* =============================
       2️⃣ CREATE VARIANTS
       ============================= */

        List<ProductVariantCreateDTO> variants =
                (dto.getVariants() == null || dto.getVariants().isEmpty())
                        ? List.of(defaultVariant(dto.getProduct()))
                        : dto.getVariants();

        List<ProductVariant> createdVariants = new ArrayList<>();

        for (ProductVariantCreateDTO v : variants) {

            v.setProductId(product.getId());

            ProductVariantDTO created =
                    productVariantService.createVariant(v);

            ProductVariant entity =
                    productVariantRepository.findById(created.getId())
                            .orElseThrow();

            createdVariants.add(entity);
        }

    /* =============================
       3️⃣ HANDLE FILES (REUSE PIPELINE)
       ============================= */

        if (files != null && !files.isEmpty()) {

            productImageService.attachFilesWithAssignments(
                    product,
                    createdVariants,
                    dto.getFileAssignments(),
                    files
            );
        }

        return productMapper.toDTO(product);
    }

    private ProductVariantCreateDTO defaultVariantDTO(UUID productId) {

        ProductVariantCreateDTO dto = new ProductVariantCreateDTO();
        dto.setProductId(productId);
        dto.setClassification("STANDARD");
        return dto;
    }

    private ProductVariantCreateDTO defaultVariant(ProductCreateDTO productCreateDTO) {

        ProductVariantCreateDTO dto = new ProductVariantCreateDTO();
        dto.setClassification("STANDARD");
        dto.setMinimumPercentageProfit(productCreateDTO.getMinimumPercentageProfit());
        dto.setMinimumProfit(productCreateDTO.getMinimumProfit());
        return dto;
    }

    @Transactional
    public ProductDTO createProductCore(UUID branchId, ProductCreateDTO dto) {

    /* =============================
       VALIDATION
    ============================= */

        if (dto.getName() == null || dto.getName().isBlank()) {
            throw new IllegalArgumentException("Product name is required.");
        }

        if (dto.getCategoryId() == null) {
            throw new IllegalArgumentException("Category is required.");
        }

    /* =============================
       UNIQUENESS (TENANT + BRANCH)
    ============================= */

        if (productRepository.existsByTenantIdAndBranchIdAndNameIgnoreCase(
                tenantId(), branchId, dto.getName()
        )) {
            throw new IllegalArgumentException(
                    "Product already exists with name: " + dto.getName()
            );
        }

    /* =============================
       CATEGORY (STRICT SAFE)
    ============================= */

        Category category = categoryRepository
                .findByIdSafe(
                        dto.getCategoryId(),
                        false,
                        tenantId(),
                        branchId
                )
                .orElseThrow(() ->
                        new IllegalArgumentException("Invalid categoryId")
                );

    /* =============================
       BUILD ENTITY
    ============================= */

        Product product = productMapper.toEntity(dto);
        product.setCategory(category);

    /* =============================
       SUPPLIERS (FIXED 🔥)
    ============================= */

        if (dto.getSupplierIds() != null && !dto.getSupplierIds().isEmpty()) {

            List<Supplier> suppliers =
                    supplierRepository.findAllByIdsSafe(
                            dto.getSupplierIds(),
                            tenantId(),
                            branchId
                    );

            if (suppliers.size() != dto.getSupplierIds().size()) {
                throw new IllegalArgumentException("Some suppliers are invalid for this tenant/branch");
            }

            final Product finalProduct = product;

            Set<ProductSupplier> productSuppliers = suppliers.stream()
                    .map(supplier -> ProductSupplier.builder()
                            .product(finalProduct)
                            .supplier(supplier)
                            .build()
                    )
                    .collect(Collectors.toSet());

            product.setSuppliers(productSuppliers);

            // keep category sync
            syncCategorySuppliers(category, new HashSet<>(suppliers));
        }

    /* =============================
       SKU (TENANT SAFE)
    ============================= */

        if (product.getSku() == null || product.getSku().isBlank()) {

            product.setSku(
                    skuService.generateSkuForCategory(category)
            );

        } else {

            if (productRepository.existsByTenantIdAndBranchIdAndSku(
                    tenantId(), branchId, product.getSku()
            )) {
                throw new IllegalArgumentException("SKU already exists");
            }
        }

    /* =============================
       SAVE
    ============================= */

        product = productRepository.save(product);

    /* =============================
       AUDIT
    ============================= */

        productAuditRepository.save(
                ProductAudit.builder()
                        .action("CREATE")
                        .productId(product.getId())
                        .productName(product.getName())
                        .timestamp(LocalDateTime.now())
                        .performedBy(SecurityUtils.currentUsername())
                        .build()
        );

        return productMapper.toDTO(product);
    }

    @Transactional
    public ProductDTO createProduct(UUID branchId, ProductCreateDTO dto) throws IOException {

        ProductFullCreateDTO full = new ProductFullCreateDTO();
        full.setProduct(dto);

        // convert simple variant string list into DTO list
        if (dto.getVariants() != null && !dto.getVariants().isEmpty()) {

            List<ProductVariantCreateDTO> variantDTOs =
                    dto.getVariants().stream()
                            .map(v -> {
                                ProductVariantCreateDTO vd = new ProductVariantCreateDTO();
                                vd.setClassification(v);
                                return vd;
                            })
                            .toList();

            full.setVariants(variantDTOs);
        }

        // No file assignments
        full.setFileAssignments(null);

        return fullCreate(branchId, full, null);
    }

    private void validateCreateDTO(ProductCreateDTO dto) {
        if (dto.getName() == null || dto.getName().isBlank()) {
            throw new IllegalArgumentException("Product name is required.");
        }
        if (dto.getCategoryId() == null) {
            throw new IllegalArgumentException("Category is required.");
        }
    }

    public String generateVariantSku(Product product, String classification) {
        return product.getSku() + "-" + classification.replaceAll(" ", "-");
    }

    public boolean existsByName(String name) {

        return productRepository.existsByTenantIdAndBranchIdAndNameIgnoreCase(
                TenantContext.getTenantId(),
                BranchContext.get(),
                name
        );
    }

    /**
     * Generate SKU for a category using first 3 letters uppercase (alpha only),
     * padded/truncated to 3 chars, then a 6-digit sequence number (per-category).
     * <p>
     * Example: ACT-000547
     */

    @Transactional
    public ProductDTO updateProduct(UUID id, ProductUpdateDTO dto) throws IOException {

        Product product = getProductOrThrow(id, dto.getBranchId());

        validateUpdateDTOUniqueness(id, dto);

        List<ProductAudit> audits = new ArrayList<>();

        updateBasicFields(product, dto, audits);
        updateCategory(product, dto, audits);
        updateSuppliers(product, dto, audits);
        updateVariants(product, dto);
        updatePricing(product, dto, audits);

        product.setUpdatedAt(LocalDateTime.now());

        Product saved = productRepository.save(product);

        if (!audits.isEmpty()) {
            productAuditRepository.saveAll(audits);
        }

        return productMapper.toDTO(saved);
    }

    private void updateBasicFields(Product product,
                                   ProductUpdateDTO dto,
                                   List<ProductAudit> audits) {

        /* NAME */
        if (dto.getName() != null && !dto.getName().equals(product.getName())) {
            audits.add(auditForField(product, "name", product.getName(), dto.getName()));
            product.setName(dto.getName());
        }

        /* DESCRIPTION */
        if (dto.getDescription() != null &&
                !Objects.equals(dto.getDescription(), product.getDescription())) {

            audits.add(auditForField(product, "description",
                    product.getDescription(), dto.getDescription()));

            product.setDescription(dto.getDescription());
        }

        /* SKU */
        if (dto.getSku() != null && !dto.getSku().equals(product.getSku())) {

            if (productRepository.existsByTenantIdAndBranchIdAndSku(
                    tenantId(), dto.getBranchId(), dto.getSku())) {
                throw new IllegalArgumentException("SKU already exists");
            }

            audits.add(auditForField(product, "sku", product.getSku(), dto.getSku()));
            product.setSku(dto.getSku());
        }
    }

    private void updateCategory(Product product,
                                ProductUpdateDTO dto,
                                List<ProductAudit> audits) {

        if (dto.getCategoryId() == null) return;

        if (product.getCategory() != null &&
                Objects.equals(product.getCategory().getId(), dto.getCategoryId())) {
            return;
        }

        Category newCat = categoryRepository.findByIdSafe(
                dto.getCategoryId(),
                false,
                tenantId(),
                dto.getBranchId()
        ).orElseThrow(() -> new IllegalArgumentException("Invalid categoryId"));

        audits.add(auditForField(
                product,
                "categoryId",
                product.getCategory() == null ? null : product.getCategory().getId().toString(),
                dto.getCategoryId().toString()
        ));

        product.setCategory(newCat);
    }

    private void updateSuppliers(Product product,
                                 ProductUpdateDTO dto,
                                 List<ProductAudit> audits) {

        if (dto.getSupplierIds() == null) return;

        List<Supplier> suppliers =
                supplierRepository.findAllByIdsSafe(
                        dto.getSupplierIds(),
                        tenantId(),
                        dto.getBranchId()
                );

        String oldSuppliers = product.getSuppliers() == null
                ? ""
                : product.getSuppliers().stream()
                .map(ps -> ps.getSupplier().getId().toString())
                .collect(Collectors.joining(","));

        String newSuppliers = suppliers.stream()
                .map(s -> s.getId().toString())
                .collect(Collectors.joining(","));

        if (Objects.equals(oldSuppliers, newSuppliers)) return;

        audits.add(auditForField(product, "supplierIds", oldSuppliers, newSuppliers));

        product.getSuppliers().clear();

        for (Supplier supplier : suppliers) {
            product.getSuppliers().add(
                    ProductSupplier.builder()
                            .product(product)
                            .supplier(supplier)
                            .build()
            );
        }

        syncCategorySuppliers(product.getCategory(), new HashSet<>(suppliers));
    }

    private void updateVariants(Product product, ProductUpdateDTO dto) {

        if (dto.getVariants() == null) return;

        for (ProductVariantCreateDTO v : dto.getVariants()) {

            ProductVariant existing =
                    productVariantRepository
                            .findByTenantIdAndBranchIdAndProduct_IdAndClassification(
                                    tenantId(),
                                    dto.getBranchId(),
                                    product.getId(),
                                    v.getClassification()
                            ).orElse(null);

            if (existing != null) continue;

            ProductVariant variant = new ProductVariant();
            variant.setProduct(product);
            variant.setClassification(v.getClassification());

            variant.setSku(
                    v.getSku() != null
                            ? v.getSku()
                            : generateVariantSku(product, v.getClassification())
            );

            productVariantRepository.save(variant);
        }
    }

    private void updatePricing(Product product,
                               ProductUpdateDTO dto,
                               List<ProductAudit> audits) {

        if (dto.getMinimumPercentageProfit() == null) return;

        if (Objects.equals(dto.getMinimumPercentageProfit(),
                product.getMinimumPercentageProfit())) return;

        audits.add(auditForField(
                product,
                "minimumPercentageProfit",
                product.getMinimumPercentageProfit() == null ? null :
                        product.getMinimumPercentageProfit().toString(),
                dto.getMinimumPercentageProfit().toString()
        ));

        product.setMinimumPercentageProfit(dto.getMinimumPercentageProfit());

        productRepository.save(product);

        // Pricing handled by PricingEngine rules. No mutation here.
    }

    private void validateUpdateDTOUniqueness(UUID productId, ProductUpdateDTO dto) {

        if (dto.getName() != null) {

            productRepository.findByTenantIdAndBranchIdAndNameIgnoreCase(
                    tenantId(),
                    dto.getBranchId(),
                    dto.getName()
            ).ifPresent(existing -> {
                if (!existing.getId().equals(productId)) {
                    throw new IllegalArgumentException(
                            "A product with the name '" + dto.getName() + "' already exists."
                    );
                }
            });
        }
    }

    private void syncCategorySuppliers(Category category, Set<Supplier> productSuppliers) {

        if (category == null || productSuppliers == null || productSuppliers.isEmpty())
            return;

        boolean changed = false;

        for (Supplier supplier : productSuppliers) {

            boolean alreadyLinked = category.getCategorySuppliers()
                    .stream()
                    .anyMatch(rel ->
                            rel.getSupplier().getId().equals(supplier.getId())
                    );

            if (!alreadyLinked) {

                CategorySupplier relation = CategorySupplier.builder()
                        .id(new CategorySupplierId(
                                category.getId(),
                                supplier.getId()
                        ))
                        .category(category)
                        .supplier(supplier)
                        .build();

                category.getCategorySuppliers().add(relation);
                changed = true;
            }
        }

        if (changed) {
            categoryRepository.save(category);
        }
    }

    /* =============================
       IMAGE HANDLING + AUDITING
       ============================= */

    @Transactional
    public void uploadProductImages(UUID branchId, UUID productId, List<MultipartFile> files) throws IOException {

        Product product = productRepository
                .findByIdAndTenantIdAndBranchId(productId, tenantId(), branchId)
                .orElseThrow(() -> new ProductNotFoundException("Product not found"));

        productImageService.saveProductImages(product, files);

        productRepository.save(product);
    }

    public void restoreProductImage(UUID branchId, UUID productId, UUID productImageId, String reason) {

        Product product = productRepository
                .findByIdAndTenantIdAndBranchId(productId, tenantId(), branchId)
                .orElseThrow(() -> new ProductNotFoundException("Product not found"));

        ProductImage img = productImageRepository.findById(productImageId)
                .orElseThrow(() -> new EntityNotFoundException("Product image not found"));

        // 🔥 ensure image belongs to this product
        if (!img.getProduct().getId().equals(productId)) {
            throw new IllegalStateException("Image does not belong to product");
        }

        img.setDeleted(false);
        productImageRepository.save(img);

        productImageAuditRepository.save(
                imageAuditForAction(product, img, "IMAGE_RESTORE_INDEPENDENTLY",
                        reason == null ? "Restored by user" : reason)
        );
    }

    public void deleteProductImageByFilename(UUID branchId, UUID productId, String filename, Boolean soft) throws IOException {

        Product product = productRepository
                .findByIdAndTenantIdAndBranchId(productId, tenantId(), branchId)
                .orElseThrow(() -> new ProductNotFoundException("Product not found"));

        if (product.getImages() == null) return;

        for (ProductImage img : new ArrayList<>(product.getImages())) {

            if (!img.getProduct().getId().equals(productId)) continue;

            if (Boolean.TRUE.equals(soft)) {

                img.setDeleted(true);
                productImageRepository.save(img);

                productImageAuditRepository.save(
                        imageAuditForAction(product, img,
                                "IMAGE_SOFT_DELETED_INDEPENDENTLY",
                                "Deleted by user")
                );

            } else {

                if (filename.equals(img.getFileName())) {

                    Path physical = resolveProductFile(product.getId(), img.getFileName());

                    boolean usedElsewhere = isFileUsedElsewhere(branchId, img, productId);

                    product.getImages().remove(img);
                    productImageRepository.delete(img);

                    if (!usedElsewhere) {
                        Files.deleteIfExists(physical);
                    }

                    productImageAuditRepository.save(
                            imageAuditForAction(product, img,
                                    "IMAGE_HARD_DELETED_INDEPENDENTLY",
                                    "Deleted by user")
                    );
                }
            }
        }

        productRepository.save(product);
    }

    public void deleteAllProductImages(UUID branchId, UUID productId, Boolean soft, String reason) throws IOException {

        Product product = productRepository
                .findByIdAndTenantIdAndBranchId(productId, tenantId(), branchId)
                .orElseThrow(() -> new ProductNotFoundException("Product not found"));

        if (product.getImages() == null) return;

        deleteProductImages(branchId, product, soft, reason);

        if (Boolean.FALSE.equals(soft)) {

            product.setImages(new ArrayList<>());
            productRepository.save(product);

            productImageAuditRepository.save(
                    imageAuditForAction(product, null,
                            "ALL_IMAGES_DELETED",
                            "All images deleted")
            );
        }
    }

    public void deleteProductImages(UUID branchId, Product product, Boolean soft, String reason) {
        for (ProductImage img : new ArrayList<>(product.getImages())) {
            if(Boolean.TRUE.equals(soft)) {
                img.setDeleted(true);
                productImageRepository.save(img);
                productImageAuditRepository.save(imageAuditForAction(product, img, "IMAGE_SOFT_DELETED_INDEPENDENTLY", reason));
            } else {
                try {
                    ProductImageAudit audit = ProductImageAudit.builder()
                            .action("IMAGE_DELETED_PERMANENTLY")
                            .fileName(img.getFileName())
                            .filePath(img.getFilePath())
                            .reason(reason)
                            .productId(product.getId())
                            .productName(product.getName())
                            .timestamp(LocalDateTime.now())
                            .performedBy(SecurityUtils.currentUsername())
                            .build();
                    productImageAuditRepository.save(audit);

                    Path physical = resolveProductFile(product.getId(), img.getFileName());

                    boolean usedElsewhere = isFileUsedElsewhere(branchId, img, product.getId());

                    if (!usedElsewhere) {
                        Files.deleteIfExists(physical);
                    }
                } catch (IOException ignored) {
                }
                productImageRepository.delete(img);
            }
        }
    }

    /* =============================
       SOFT / RESTORE / HARD DELETE (with audits)
       ============================= */

    @Transactional
    public void softDeleteProduct(UUID productId, String reason) {

        Product product = getProductOrThrow(productId, null);
        UUID branchId = product.getBranchId();

        if (Boolean.TRUE.equals(product.getDeleted())) {
            return;
        }

        LocalDateTime now = LocalDateTime.now();
        String username = SecurityUtils.currentUsername();

    /* ===========================
       FETCH EVERYTHING (TENANT SAFE)
       =========================== */

        List<ProductImage> productImages =
                productImageRepository.findByTenantIdAndBranchIdAndProduct_Id(
                        tenantId(), branchId, productId
                );

        List<ProductVariant> variants =
                productVariantRepository.findByTenantIdAndBranchIdAndProduct_Id(
                        tenantId(), branchId, productId
                );

        List<UUID> variantIds = variants.stream()
                .map(ProductVariant::getId)
                .toList();

        List<ProductVariantImage> variantImages =
                variantIds.isEmpty()
                        ? List.of()
                        : productVariantImageRepository.findByVariantIdsWithVariant(variantIds, tenantId(), branchId);

    /* ===========================
       APPLY SOFT DELETE
       =========================== */

        product.setDeleted(true);
        product.setDeletedAt(now);

        productImageRepository.softDeleteByProductId(productId, tenantId(), branchId);

        if (!variantIds.isEmpty()) {
            productVariantRepository.softDeleteByProductId(productId, tenantId(), branchId);
            productVariantImageRepository.softDeleteByVariantIds(variantIds, tenantId(), branchId);
            inventoryItemRepository.softDeleteByVariantIds(variantIds, tenantId());
            stockTransactionRepository.softDeleteByVariantIds(variantIds, tenantId());
        }

        productRepository.save(product);

    /* ===========================
       AUDITS
       =========================== */

        productAuditRepository.save(
                ProductAudit.builder()
                        .action("SOFT_DELETE")
                        .productId(product.getId())
                        .productName(product.getName())
                        .timestamp(now)
                        .performedBy(username)
                        .reason(reason)
                        .build()
        );

        auditProductImages(productImages,
                "IMAGE_SOFT_DELETED_ALONG_WITH_PRODUCT",
                product,
                reason,
                now,
                username
        );

        auditVariants(variants,
                "VARIANT_SOFT_DELETED_ALONG_WITH_PRODUCT",
                product,
                reason,
                now,
                username
        );

        auditVariantImages(variantImages,
                "VARIANT_IMAGE_SOFT_DELETED_ALONG_WITH_PRODUCT",
                product,
                reason,
                now,
                username
        );
    }

/* =========================================================
    BULK SOFT DELETE (All or Nothing)
    ========================================================= */

    @Transactional
    public void bulkSoftDelete(List<UUID> ids, String reason) {
        for (UUID id : ids) {
            softDeleteProduct(id, reason);
        }
    }

/* =========================================================
    RESTORE (Single)
    ========================================================= */

    @Transactional
    public void restoreProduct(UUID productId,
                               String reason,
                               ProductRestoreOptions options) {

        if (options != null &&
                options.isRestoreInventory() &&
                !options.isRestoreStockTransactions()) {

            throw new IllegalArgumentException(
                    "Cannot restore inventory without restoring stock transactions."
            );
        }

        Product product = getProductOrThrow(productId, null);
        UUID branchId = product.getBranchId();

        if (!Boolean.TRUE.equals(product.getDeleted())) {
            return;
        }

        LocalDateTime now = LocalDateTime.now();
        String username = SecurityUtils.currentUsername();

    /* ===========================
       RESTORE PRODUCT
       =========================== */

        product.setDeleted(false);
        product.setDeletedAt(null);
        productRepository.save(product);

        productAuditRepository.save(
                ProductAudit.builder()
                        .action("RESTORE")
                        .productId(product.getId())
                        .productName(product.getName())
                        .branchId(branchId)
                        .tenantId(tenantId())
                        .timestamp(now)
                        .performedBy(username)
                        .reason(reason)
                        .build()
        );

    /* ===========================
       PRODUCT IMAGES
       =========================== */

        productImageRepository.restoreByProductId(productId, tenantId(), branchId);

        List<ProductImage> productImages =
                productImageRepository.findByTenantIdAndBranchIdAndProduct_Id(
                        tenantId(), branchId, productId
                );

        auditProductImages(productImages,
                "IMAGE_RESTORED_ALONG_WITH_PRODUCT",
                product,
                reason,
                now,
                username
        );

    /* ===========================
       VARIANTS
       =========================== */

        List<ProductVariant> variants =
                productVariantRepository.findByTenantIdAndBranchIdAndProduct_Id(
                        tenantId(), branchId, productId
                );

        if (!variants.isEmpty()) {

            List<UUID> variantIds = variants.stream()
                    .map(ProductVariant::getId)
                    .toList();

            productVariantRepository.restoreByProductId(productId, tenantId(), branchId);

            auditVariants(variants,
                    "VARIANT_RESTORED_ALONG_WITH_PRODUCT",
                    product,
                    reason,
                    now,
                    username
            );

        /* ===========================
           VARIANT IMAGES
           =========================== */

            List<ProductVariantImage> variantImages =
                    productVariantImageRepository.findByVariantIdsWithVariant(variantIds, tenantId(), branchId);

            List<ProductVariantImage> deletedVariantImages =
                    variantImages.stream()
                            .filter(img -> Boolean.TRUE.equals(img.getDeleted()))
                            .toList();

            productVariantImageRepository.restoreByVariantIds(variantIds, tenantId(), branchId);

            auditVariantImages(
                    deletedVariantImages,
                    "VARIANT_IMAGE_RESTORED_ALONG_WITH_PRODUCT",
                    product,
                    reason,
                    now,
                    username
            );

        /* ===========================
           INVENTORY + STOCK
           =========================== */

            if (options != null) {

                if (options.isRestoreStockTransactions()) {
                    stockTransactionRepository.restoreByVariantIds(
                            variantIds,
                            tenantId(),
                            branchId
                    );
                }

                if (options.isRestoreInventory()) {
                    inventoryItemRepository.restoreByVariantIds(variantIds, tenantId(), branchId);
                    recalculateInventoryFromTransactions(variantIds, username, branchId);
                }
            }
        }
    }

/* =========================================================
    BULK RESTORE
    ========================================================= */

    @Transactional
    public void bulkRestore(List<UUID> ids,
                            String reason,
                            ProductRestoreOptions options) {
        for (UUID id : ids) {
            restoreProduct(id, reason, options);
        }
    }

/* =========================================================
    INVENTORY RECALCULATION
    ========================================================= */

    private void recalculateInventoryFromTransactions(
            List<UUID> variantIds,
            String username,
            UUID branchId
    ) {

        for (UUID variantId : variantIds) {

            List<StockTransaction> transactions =
                    stockTransactionRepository.findByProductVariantIdAndBranchIdAndTenantIdOrderByTimestampDesc(variantId, tenantId(), branchId);

            long quantity = transactions.stream()
                    .mapToLong(StockTransaction::getQuantityDelta)
                    .sum();

            InventoryItem item =
                    inventoryItemRepository.findByProductVariantIdAndTenantIdAndBranchId(variantId, tenantId(), branchId)
                            .orElseThrow(() -> new EntityNotFoundException("Inventory for variant " + variantId));

            item.setDeleted(false);
            item.setQuantityOnHand(quantity);
            item.setLastUpdatedAt(LocalDateTime.now());
            item.setLastUpdatedBy(username);

            inventoryItemRepository.save(item);
        }
    }

/* =========================================================
    PRIVATE HELPERS
    ========================================================= */

    private Product getProductOrThrow(UUID id, UUID branchId) {

        return productRepository
                .findByIdAndTenantIdAndBranchId(id, tenantId(), branchId == null ? branchId : branchId)
                .orElseThrow(() -> new ProductNotFoundException("Product not found"));
    }

    private void auditProductImages(List<ProductImage> images,
                                    String action,
                                    Product product,
                                    String reason,
                                    LocalDateTime now,
                                    String username) {

        if (images.isEmpty()) return;

        productImageAuditRepository.saveAll(
                images.stream()
                        .map(img -> ProductImageAudit.builder()
                                .action(action)
                                .fileName(img.getFileName())
                                .filePath(img.getFilePath())
                                .productId(product.getId())
                                .productName(product.getName())
                                .timestamp(now)
                                .performedBy(username)
                                .reason(reason)
                                .build())
                        .toList()
        );
    }

    private void auditVariants(List<ProductVariant> variants,
                               String action,
                               Product product,
                               String reason,
                               LocalDateTime now,
                               String username) {

        if (variants.isEmpty()) return;

        productVariantAuditRepository.saveAll(
                variants.stream()
                        .map(v -> ProductVariantAudit.builder()
                                .action(action)
                                .productId(product.getId())
                                .productName(product.getName())
                                .productVariantId(v.getId())
                                .variantClassification(v.getClassification())
                                .timestamp(now)
                                .performedBy(username)
                                .reason(reason)
                                .build())
                        .toList()
        );
    }

    private void auditVariantImages(List<ProductVariantImage> images,
                                    String action,
                                    Product product,
                                    String reason,
                                    LocalDateTime now,
                                    String username) {

        if (images.isEmpty()) return;

        productVariantImageAuditRepository.saveAll(
                images.stream()
                        .map(img -> ProductVariantImageAudit.builder()
                                .productVariantId(img.getVariant().getId())
                                .productName(product.getName())
                                .classification(img.getVariant().getClassification())
                                .fileName(img.getFileName())
                                .filePath(img.getFilePath())
                                .action(action)
                                .reason(reason)
                                .timestamp(now)
                                .performedBy(username)
                                .build())
                        .toList()
        );
    }

//    @Transactional
//    public void hardDeleteProduct(UUID productId, String reason) throws IOException {
//
//        Product product = productRepository.findById(productId)
//                .orElseThrow(() -> new ProductNotFoundException("Product not found"));
//
//        // === HARD DELETE AUDIT BEFORE REMOVAL ===
//        ProductAudit audit = new ProductAudit();
//        audit.setAction("HARD_DELETE");
//        audit.setProductId(product.getId());
//        audit.setProductName(product.getName());
//        audit.setTimestamp(LocalDateTime.now());
//        audit.setPerformedBy(SecurityUtils.currentUsername());
//        audit.setReason(reason);
//        productAuditRepository.save(audit);
//
//        // === VARIANTS ===
//        List<ProductVariant> variants =
//                productVariantRepository.findByProduct_Id(productId);
//
//        List<UUID> variantIds =
//                variants.stream()
//                        .map(ProductVariant::getId)
//                        .toList();
//
//        // === SALES ===
//        if (!variantIds.isEmpty()) {
//            saleLineItemRepository.deleteAllByProductVariantIdIn(variantIds);
//            saleRepository.deleteSalesByVariantIds(variantIds);
//        }
//
//        // === STOCK ===
//        stockTransactionRepository.deleteAllByProductId(productId);
//
//        // === INVENTORY ===
//        inventoryItemRepository.deleteAllByProductId(productId);
//
//        // === VARIANTS CLEANUP ===
//        for (ProductVariant v : variants) {
//            v.getImages().clear();
//        }
//
//        productVariantRepository.deleteAllByProduct_Id(productId);
//
//        // === PRODUCT IMAGES ===
//        deleteProductImages(product, false, "Deleted along with product");
//
//        // === PRODUCT ===
//        productRepository.delete(product);
//    }

    @Transactional
    public void hardDeleteProduct(UUID branchId, UUID productId, String reason) {

        Product product = getProductForUpdate(branchId, productId);

        LocalDateTime now = LocalDateTime.now();
        String username = SecurityUtils.currentUsername();

        List<ProductImage> productImages = fetchProductImages(branchId, productId);
        List<ProductVariant> variants = fetchVariants(branchId, productId);
        List<UUID> variantIds = extractVariantIds(variants);
        List<ProductVariantImage> variantImages = fetchVariantImages(branchId, variantIds);

        validateHardDeleteSafety(branchId, productId, variantIds);

        auditHardDelete(product, productImages, variants, variantImages, reason, now, username);

        List<Path> filesToDelete = collectFilesToDelete(branchId, productImages, variantImages, productId);

        deleteRelations(branchId, productId, variantIds);

        scheduleFileCleanup(filesToDelete);
    }

    @Transactional
    public void bulkHardDelete(UUID branchId, List<UUID> ids, String reason) {
        int i = 0;
        for (UUID id : ids) {

            hardDeleteProduct(branchId, id, reason);

            if (++i % 20 == 0) {
                em.flush();
                em.clear();
            }
        }
    }

    private Product getProductForUpdate(UUID branchId, UUID productId) {
        return productRepository.findForUpdate(productId, tenantId(), branchId)
                .orElseThrow(() -> new ProductNotFoundException("Product not found"));
    }

    private List<ProductImage> fetchProductImages(UUID branchId, UUID productId) {
        return productImageRepository.findByTenantIdAndBranchIdAndProduct_Id(
                tenantId(), branchId, productId
        );
    }

    private List<ProductVariant> fetchVariants(UUID branchId, UUID productId) {
        return productVariantRepository.findByTenantIdAndBranchIdAndProduct_Id(
                tenantId(), branchId, productId
        );
    }

    private List<UUID> extractVariantIds(List<ProductVariant> variants) {
        return variants.stream().map(ProductVariant::getId).toList();
    }

    private List<ProductVariantImage> fetchVariantImages(UUID branchId, List<UUID> variantIds) {
        return variantIds.isEmpty()
                ? List.of()
                : productVariantImageRepository.findByVariantIdsWithVariant(
                variantIds, tenantId(), branchId
        );
    }

    private void validateHardDeleteSafety(UUID branchId, UUID productId, List<UUID> variantIds) {

        if (!variantIds.isEmpty()) {

            boolean hasCompletedSales =
                    saleRepository.existsByVariantIdsAndStatus(
                            variantIds,
                            Sale.SaleStatus.COMPLETED
                    );

            if (hasCompletedSales) {
                throw new IllegalStateException("Cannot delete product with completed sales");
            }
        }

        if (stockTransactionRepository.existsByProductIdAndTenantIdAndBranchId(
                productId, tenantId(), branchId
        ) ||
                inventoryItemRepository.existsByProductIdAndTenantIdAndBranchId(
                        productId, tenantId(), branchId
                )) {

            throw new IllegalStateException("Cannot delete product with inventory history");
        }
    }

    private void auditHardDelete(
            Product product,
            List<ProductImage> productImages,
            List<ProductVariant> variants,
            List<ProductVariantImage> variantImages,
            String reason,
            LocalDateTime now,
            String username
    ) {

        productAuditRepository.save(
                auditFactory("HARD_DELETE", product, reason, now, username)
        );

        auditProductImages(productImages, "IMAGE_DELETED_PERMANENTLY", product, reason, now, username);
        auditVariants(variants, "VARIANT_HARD_DELETED", product, reason, now, username);
        auditVariantImages(variantImages, "VARIANT_IMAGE_DELETED_PERMANENTLY", product, reason, now, username);
    }

    private List<Path> collectFilesToDelete(
            UUID branchId,
            List<ProductImage> productImages,
            List<ProductVariantImage> variantImages,
            UUID productId
    ) {

        Path sharedDir = fileStorageService.productSharedRoot();

        List<Path> files = new ArrayList<>();

        for (ProductImage img : productImages) {

            boolean usedElsewhere = productImageRepository
                    .existsByTenantIdAndBranchIdAndContentHashAndProduct_IdNot(
                            tenantId(), branchId, img.getContentHash(), productId
                    );

            if (!usedElsewhere) {
                files.add(sharedDir.resolve(img.getFileName()));
            }
        }

        variantImages.forEach(img ->
                files.add(sharedDir.resolve(img.getFileName()))
        );

        return files;
    }

    private void deleteRelations(UUID branchId, UUID productId, List<UUID> variantIds) {

        productRepository.detachSuppliers(productId);

        if (!variantIds.isEmpty()) {
            productVariantImageRepository.deleteByVariantIds(variantIds, tenantId(), branchId);
            productVariantRepository.deleteByProductId(productId, tenantId(), branchId);
        }

        productImageRepository.deleteByProductId(productId, tenantId(), branchId);

        em.flush();
        em.clear();

        productRepository.deleteById(productId);
    }

    private void scheduleFileCleanup(List<Path> files) {

        transactionalFileManager.runAfterCommit(() -> {
            files.forEach(file -> {
                try {
                    fileStorageService.deleteFile(file);
                } catch (Exception e) {
                    log.warn("Failed to delete file {}", file, e);
                }
            });
        });
    }


















    /* =============================
    PRODUCT IMAGES
   ============================= */

    public ResponseEntity<Resource> downloadProductImagesZip(UUID branchId, UUID productId, Boolean deleted)
            throws IOException {

        File zip = zipProductImages(branchId, productId, deleted);

        Resource res = new FileSystemResource(zip);

        transactionalFileManager.runAfterCommit(zip::delete);

        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .header(HttpHeaders.CONTENT_DISPOSITION,
                        "attachment; filename=product-" + productId + "-images.zip")
                .body(res);
    }

    public ResponseEntity<Resource> downloadAllProductImagesZip(
            UUID branchId,
            Boolean deletedProducts,
            Boolean deletedImages
    ) throws IOException {

        File zip = zipAllProductImages(branchId, deletedProducts, deletedImages);

        Resource res = new FileSystemResource(zip);

        transactionalFileManager.runAfterCommit(zip::delete);

        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .header(HttpHeaders.CONTENT_DISPOSITION,
                        "attachment; filename=all-product-images.zip")
                .body(res);
    }

    public List<String> getProductImageUrls(UUID branchId, UUID id, Boolean deleted) {

        Product product = getProduct(branchId, id);

        if (product.getImages() == null) return List.of();

        return product.getImages()
                .stream()
                .filter(image -> matchesDeleted(deleted, image.getDeleted()))
                .map(ProductImage::getFilePath)
                .toList();
    }

    public File zipProductImages(UUID branchId, UUID productId, Boolean deleted) throws IOException {

        Product product = getProduct(branchId, productId);

        List<ProductImage> imagesToZip = product.getImages() == null
                ? List.of()
                : product.getImages().stream()
                .filter(image -> matchesDeleted(deleted, image.getDeleted()))
                .toList();

        File zipFile = File.createTempFile("product-" + productId + "-images", ".zip");

        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(zipFile))) {

            for (ProductImage image : imagesToZip) {

                Path physicalPath = resolveProductFile(
                        product.getId(),
                        Paths.get(image.getFilePath()).getFileName().toString()
                );

                addToZip(zos, physicalPath, physicalPath.getFileName().toString());
            }
        }

        return zipFile;
    }

    public Map<UUID, List<String>> getAllProductImageUrls(UUID branchId) {

        return productImageRepository
                .findAllImagePaths(tenantId(), branchId)
                .stream()
                .collect(Collectors.groupingBy(
                        ProductImageProjection::getProductId,
                        Collectors.mapping(ProductImageProjection::getFilePath, Collectors.toList())
                ));
    }

    public File zipAllProductImages(UUID branchId, Boolean deletedProducts, Boolean deletedImages) throws IOException {

        List<Product> products = productRepository.findAllWithRelationsByTenantIdAndBranchId(
                tenantId(),
                branchId
        );

        File zipFile = File.createTempFile("all-product-images", ".zip");

        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(zipFile))) {

            for (Product product : products) {

                if (deletedProducts != null) {
                    if (deletedProducts && !Boolean.TRUE.equals(product.getDeleted())) continue;
                    if (!deletedProducts && Boolean.TRUE.equals(product.getDeleted())) continue;
                }

                List<ProductImage> images = product.getImages() == null
                        ? List.of()
                        : product.getImages();

                for (ProductImage image : images) {

                    if (!matchesDeleted(deletedImages, image.getDeleted())) continue;

                    Path physicalPath = resolveProductFile(
                            product.getId(),
                            Paths.get(image.getFilePath()).getFileName().toString()
                    );

                    String entryName = product.getId() + "/" + physicalPath.getFileName();

                    addToZip(zos, physicalPath, entryName);
                }
            }
        }

        return zipFile;
    }

    public ResponseEntity<Resource> getProductThumbnail(UUID branchId, UUID id) {

        Product product = productRepository
                .findByIdAndTenantIdAndBranchIdAndDeletedFalse(
                        id,
                        tenantId(),
                        branchId
                )
                .orElseThrow(() -> new ProductNotFoundException("Product not found"));

        if (product.getImages() == null || product.getImages().isEmpty()) {
            return ResponseEntity.notFound().build();
        }

        ProductImage primary = product.getImages().stream()
                .filter(img -> !Boolean.TRUE.equals(img.getDeleted()))
                .filter(img -> Boolean.TRUE.equals(img.getPrimaryImage()))
                .findFirst()
                .orElseGet(() ->
                        product.getImages().stream()
                                .filter(img -> !Boolean.TRUE.equals(img.getDeleted()))
                                .findFirst()
                                .orElse(null)
                );

        if (primary == null || primary.getThumbnailFileName() == null) {
            return ResponseEntity.notFound().build();
        }

        Path path = fileStorageService.productSharedRoot()
                .resolve(primary.getThumbnailFileName());

        if (!Files.exists(path)) {
            return ResponseEntity.notFound().build();
        }

        Resource resource = new FileSystemResource(path.toFile());

        return ResponseEntity.ok()
                .contentType(MediaType.IMAGE_JPEG)  // ✅ RESTORED
                .header(HttpHeaders.CACHE_CONTROL, "public, max-age=86400") // ✅ RESTORED
                .body(resource);
    }




    /* =============================
       PRODUCT AUDITS
       ============================= */

    public List<ProductAudit> getProductAudits(UUID branchId, UUID productId) {

        return productAuditRepository
                .findByTenantIdAndBranchIdAndProductIdOrderByTimestampDesc(
                        tenantId(),
                        branchId,
                        productId
                );
    }

    public List<ProductImageAudit> getProductImagesAudits(UUID branchId, UUID productId) {

        return productImageAuditRepository
                .findByTenantIdAndBranchIdAndProductId(
                        tenantId(),
                        branchId,
                        productId
                )
                .stream()
                .sorted(Comparator.comparing(ProductImageAudit::getTimestamp).reversed())
                .toList();
    }

    public Map<UUID, List<ProductImageAudit>> getAllProductImagesAudits(UUID branchId) {

        return productImageAuditRepository
                .findByTenantIdAndBranchId(tenantId(), branchId)
                .stream()
                .sorted(Comparator.comparing(ProductImageAudit::getTimestamp).reversed())
                .collect(Collectors.groupingBy(ProductImageAudit::getProductId));
    }












    /* =============================
       UTILITIES
       ============================= */

    private boolean matchesDeleted(Boolean filter, Boolean actual) {
        if (filter == null) return true;
        return filter
                ? Boolean.TRUE.equals(actual)
                : Boolean.FALSE.equals(actual);
    }

    private Product getProduct(UUID branchId, UUID id) {
        return productRepository
                .findByIdAndTenantIdAndBranchId(id, tenantId(), branchId)
                .orElseThrow(() -> new ProductNotFoundException("Product not found: " + id));
    }

    private Path resolveProductFile(UUID productId, String filename) {
        return fileStorageService.productSharedRoot()
                .resolve(filename);
    }

    private void addToZip(ZipOutputStream zos, Path file, String entryName) throws IOException {
        if (!Files.exists(file)) return;

        zos.putNextEntry(new ZipEntry(entryName));
        Files.copy(file, zos);
        zos.closeEntry();
    }

    private boolean isFileUsedElsewhere(UUID branchId, ProductImage img, UUID currentProductId) {

        return productImageRepository
                .existsByTenantIdAndBranchIdAndContentHashAndProduct_IdNot(
                        tenantId(),
                        branchId,
                        img.getContentHash(),
                        currentProductId
                );
    }

    private ProductAudit auditFactory(
            String action,
            Product product,
            String reason,
            LocalDateTime now,
            String username
    ) {

        ProductAudit audit = new ProductAudit();
        audit.setAction(action);
        audit.setProductId(product.getId());
        audit.setProductName(product.getName());
        audit.setTimestamp(now);
        audit.setPerformedBy(username);
        audit.setReason(reason);
        return audit;
    }

    private ProductAudit auditForField(Product product, String field, String oldVal, String newVal) {
        ProductAudit a = new ProductAudit();
        a.setAction("UPDATE");
        a.setFieldChanged(field);
        a.setOldValue(oldVal);
        a.setNewValue(newVal);
        a.setProductId(product.getId());
        a.setProductName(product.getName());
        a.setTimestamp(LocalDateTime.now());
        a.setPerformedBy(SecurityUtils.currentUsername());
        return a;
    }

    private ProductImageAudit imageAuditForAction(Product product, ProductImage img, String action, String reason) {
        ProductImageAudit ia = new ProductImageAudit();
        ia.setAction(action);
        ia.setReason(reason);
        ia.setProductId(product.getId());
        ia.setProductName(product.getName());
        ia.setTimestamp(LocalDateTime.now());
        ia.setPerformedBy(SecurityUtils.currentUsername());
        if (img != null) {
            ia.setFileName(img.getFileName());
            ia.setFilePath(img.getFilePath());
        }
        return ia;
    }
}