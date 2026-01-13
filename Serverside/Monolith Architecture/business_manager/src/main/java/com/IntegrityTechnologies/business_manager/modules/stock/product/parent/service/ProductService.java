package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import com.IntegrityTechnologies.business_manager.config.FileStorageProperties;
import com.IntegrityTechnologies.business_manager.config.FileStorageService;
import com.IntegrityTechnologies.business_manager.config.TransactionalFileManager;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.exception.ProductNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.repository.SupplierRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.stock.category.repository.CategoryRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.category.service.CategoryService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductCreateDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductUpdateDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.mapper.ProductMapper;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductImageAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductImageRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.dto.ProductVariantCreateDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.service.ProductVariantService;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
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
    private final FileStorageProperties fileStorageProperties;
    private final FileStorageService fileStorageService;
    private final TransactionalFileManager transactionalFileManager;
    private final ProductAuditRepository productAuditRepository;
    private final ProductImageAuditRepository productImageAuditRepository;
    private final CategoryRepository categoryRepository;
    private final SupplierRepository supplierRepository;
    private final CategoryService categoryService;
    private final ProductVariantService productVariantService;
    private final ProductVariantRepository productVariantRepository;

    private Path productRoot() {
        return Paths.get(fileStorageProperties.getProductUploadDir()).toAbsolutePath().normalize();
    }

    /* =============================
       READ METHODS (unchanged)
       ============================= */

    public List<ProductDTO> getAllProducts(Boolean deleted) {
        return productRepository.findAll().stream()
                .filter(product ->
                        deleted == null
                                ? true                                       // return all
                                : deleted                                     // deleted = true → get deleted only
                                ? Boolean.TRUE.equals(product.getDeleted())
                                : Boolean.FALSE.equals(product.getDeleted()) // deleted = false → get active only
                )
                .map(productMapper::toDTO)
                .collect(Collectors.toList());
    }


    public Page<ProductDTO> getProductsAdvanced(
            List<Long> categoryIds, String name, String description,
            BigDecimal minPrice, BigDecimal maxPrice,
            int page, int size, String sortBy, String direction, boolean includeDeleted
    ) {
        Sort sort = direction.equalsIgnoreCase("desc") ? Sort.by(sortBy).descending() : Sort.by(sortBy).ascending();
        Pageable pageable = PageRequest.of(page, size, sort);
        Specification<Product> spec = ProductSpecification.filterProducts(categoryIds, name, description, minPrice, maxPrice);
        if (!includeDeleted) spec = spec.and((r, q, cb) -> cb.isFalse(r.get("deleted")));
        return productRepository.findAll(spec, pageable).map(productMapper::toDTO);
    }

    public ProductDTO getProductById(UUID id, Boolean deleted) {
        Product p = productRepository.findById(id)
                .filter(product ->
                        deleted == null
                                ? true                                       // return all
                                : deleted                                     // deleted = true → get deleted only
                                ? Boolean.TRUE.equals(product.getDeleted())
                                : Boolean.FALSE.equals(product.getDeleted()) // deleted = false → get active only
                )
                .orElseThrow(() -> new ProductNotFoundException("Product not found: " + id));
        return productMapper.toDTO(p);
    }

    public ProductDTO getProductBySKU(String sku, Boolean deleted) {
        return productRepository.findBySku(sku)
                .filter(product ->
                        deleted == null
                                ? true                                       // return all
                                : deleted                                     // deleted = true → get deleted only
                                ? Boolean.TRUE.equals(product.getDeleted())
                                : Boolean.FALSE.equals(product.getDeleted()) // deleted = false → get active only
                )
                .map(productMapper::toDTO)
                .orElseThrow(() -> new ProductNotFoundException("Product with SKU not found: " + sku));
    }

    public List<ProductDTO> getProductsBySupplier(UUID supplierId, Boolean deleted) {
        List<Product> products = productRepository.findAllBySuppliers_Id(supplierId);
        if (products.isEmpty())
            throw new ProductNotFoundException("No products found for supplier: " + supplierId);

        return products.stream()
                .filter(product ->
                        deleted == null
                                ? true                                       // return all
                                : deleted                                     // deleted = true → get deleted only
                                ? Boolean.TRUE.equals(product.getDeleted())
                                : Boolean.FALSE.equals(product.getDeleted()) // deleted = false → get active only
                )
                .map(productMapper::toDTO)
                .toList();
    }

    public List<ProductDTO> getProductsByCategory(Long categoryId, Boolean deleted, Boolean strict) {

        List<Product> products;

        if (Boolean.TRUE.equals(strict)) {
            // Strict → products ONLY in this category
            products = productRepository.findAllByCategory_Id(categoryId);

        } else {
            // Non-strict → get products in all subcategories (recursive)
            List<Long> categoryIds = categoryService.getAllCategoryIdsRecursive(categoryId);
            products = productRepository.findAllByCategory_IdIn(categoryIds);
        }

        if (products.isEmpty())
            throw new ProductNotFoundException("No products found for category: " + categoryId);

        return products.stream()
                .filter(product ->
                        deleted == null
                                ? true
                                : deleted
                                ? Boolean.TRUE.equals(product.getDeleted())
                                : Boolean.FALSE.equals(product.getDeleted())
                )
                .map(productMapper::toDTO)
                .toList();
    }

    /* =============================
       CREATE / UPDATE (with audits & SKU gen)
       ============================= */

    @Transactional
    public ProductDTO createProduct(ProductCreateDTO dto) throws IOException {
        validateCreateDTO(dto);
        validateCreateDTOUniqueness(dto);

        // --- Map DTO to entity
        Product product = productMapper.toEntity(dto);

        // --- Category
        Category category = categoryRepository.findById(dto.getCategoryId())
                .orElseThrow(() -> new IllegalArgumentException("Invalid categoryId"));
        product.setCategory(category);

        // --- Suppliers
        if (dto.getSupplierIds() != null && !dto.getSupplierIds().isEmpty()) {
            List<Supplier> suppliers = supplierRepository.findAllById(dto.getSupplierIds());

            if (suppliers.size() != dto.getSupplierIds().size()) {
                Set<UUID> found = suppliers.stream().map(Supplier::getId).collect(Collectors.toSet());
                dto.getSupplierIds().stream()
                        .filter(id -> !found.contains(id))
                        .findFirst()
                        .ifPresent(id -> { throw new EntityNotFoundException("Supplier not found: " + id); });
            }

            // Safe assignment to avoid ConcurrentModificationException
            product.setSuppliers(new HashSet<>(suppliers));
            syncCategorySuppliers(category, product.getSuppliers());
        }

        // --- SKU
        if (product.getSku() == null || product.getSku().isBlank()) {
            product.setSku(skuService.generateSkuForCategory(category));
        } else if (productRepository.existsBySku(product.getSku())) {
            throw new IllegalArgumentException("SKU already exists");
        }

        // --- Persist product (to get ID for images)
        product = productRepository.save(product);

        // --- Create variants if provided
        if (dto.getVariants() != null && !dto.getVariants().isEmpty()) {

            for (String classification : dto.getVariants()) {
                if (classification == null || classification.isBlank()) continue;

                ProductVariant variant = new ProductVariant();
                variant.setProduct(product);
                variant.setClassification(classification);

                // no buying price yet — will be set after receiveStock
                variant.setAverageBuyingPrice(BigDecimal.ZERO);

                // Compute minimum selling price using profit percentage
                BigDecimal minBuy = variant.getAverageBuyingPrice();
                BigDecimal minSelling = productVariantService.computeMinSelling(minBuy, product.getMinimumPercentageProfit());
                variant.setMinimumSellingPrice(minSelling);

                // Generate variant SKU
                variant.setSku(generateVariantSku(product, classification));

                product.getVariants().add(variant); // ensure Product has variants list
            }

            productRepository.save(product);
        }

        // --- Save product images
        List<MultipartFile> imageFiles = dto.getImages();
        if (imageFiles != null && !imageFiles.isEmpty()) {
            saveProductImages(product, imageFiles);
        }

        // --- Save final product state
        product = productRepository.save(product);

        // --- Product audit (keep your original logic)
        ProductAudit createAudit = new ProductAudit();
        createAudit.setAction("CREATE");
        createAudit.setFieldChanged(null);
        createAudit.setOldValue(null);
        createAudit.setNewValue("created");
        createAudit.setProductId(product.getId());
        createAudit.setProductName(product.getName());
        createAudit.setTimestamp(LocalDateTime.now());
        createAudit.setPerformedBy(SecurityUtils.currentUsername());
        productAuditRepository.save(createAudit);

        log.info("Created product {} (id={} sku={}",
                product.getName(), product.getId(), product.getSku());

        return productMapper.toDTO(product);
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
        return product.getSku() + "-" + classification.replaceAll(" ","-");
    }

    private void validateCreateDTOUniqueness(ProductCreateDTO dto) {

        // --- Name (unique)
        if (dto.getName() != null && productRepository.existsByName(dto.getName())) {
            throw new IllegalArgumentException("A product with the name '" + dto.getName() + "' already exists.");
        }
    }

    /**
     * Generate SKU for a category using first 3 letters uppercase (alpha only),
     * padded/truncated to 3 chars, then a 6-digit sequence number (per-category).
     *
     * Example: ACT-000547
     */

    @Transactional
    public ProductDTO updateProduct(UUID id, ProductUpdateDTO dto) throws IOException {
        Product product = productRepository.findById(id)
                .orElseThrow(() -> new ProductNotFoundException("Product not found: " + id));

        List<ProductAudit> fieldAudits = new ArrayList<>();
        validateUpdateDTOUniqueness(dto);

        // name
        if (dto.getName() != null && !dto.getName().equals(product.getName())) {
            fieldAudits.add(auditForField(product, "name", product.getName(), dto.getName()));
            product.setName(dto.getName());
        }

        // description
        if (dto.getDescription() != null && !Objects.equals(dto.getDescription(), product.getDescription())) {
            fieldAudits.add(auditForField(product, "description", product.getDescription(), dto.getDescription()));
            product.setDescription(dto.getDescription());
        }

        // sku
        if (dto.getSku() != null && !dto.getSku().equals(product.getSku())) {
            if (productRepository.existsBySku(dto.getSku()))
                throw new IllegalArgumentException("SKU already exists");
            fieldAudits.add(auditForField(product, "sku", product.getSku(), dto.getSku()));
            product.setSku(dto.getSku());
        }

        // category change -> do not change SKU automatically, but allow category update if provided
        if (dto.getCategoryId() != null && (product.getCategory() == null || !Objects.equals(product.getCategory().getId(), dto.getCategoryId()))) {
            Category newCat = categoryRepository.findById(dto.getCategoryId()).orElseThrow(() -> new IllegalArgumentException("Invalid categoryId"));
            fieldAudits.add(auditForField(product, "categoryId", product.getCategory() == null ? null : product.getCategory().getId().toString(), dto.getCategoryId().toString()));
            product.setCategory(newCat);
        }

        // suppliers update — patching simple replacement
        if (dto.getSupplierIds() != null) {
            Set<Supplier> newSuppliers = supplierRepository.findAllById(dto.getSupplierIds()).stream().collect(Collectors.toSet());
            String oldSuppliers = product.getSuppliers() == null ? "" : product.getSuppliers().stream().map(Supplier::getId).map(UUID::toString).collect(Collectors.joining(","));
            String newSuppliersStr = newSuppliers.stream().map(Supplier::getId).map(UUID::toString).collect(Collectors.joining(","));
            if (!Objects.equals(oldSuppliers, newSuppliersStr)) {
                fieldAudits.add(auditForField(product, "supplierIds", oldSuppliers, newSuppliersStr));
                product.setSuppliers(newSuppliers);
                syncCategorySuppliers(product.getCategory(), newSuppliers);
            }
        }

        product.setUpdatedAt(LocalDateTime.now());

        ProductVariant variant;

        for(ProductVariantCreateDTO variantCreateDTO: dto.getVariants()) {
            variant = productVariantRepository.findByProductIdAndClassification(
                    id, variantCreateDTO.getClassification()
            ).orElse(null);

            if (variant == null) {
                variant = new ProductVariant();
                variant.setProduct(product);
                variant.setAverageBuyingPrice(BigDecimal.ZERO);
                variant.setClassification(variantCreateDTO.getClassification());
                variant.setSku(
                        variantCreateDTO.getSku() != null ?
                                variantCreateDTO.getSku() :
                                generateVariantSku(product, variantCreateDTO.getClassification())
                );

                productVariantRepository.save(variant);
            }
        }

        // minimumPercentageProfit changed?
        if (dto.getMinimumPercentageProfit() != null &&
                !Objects.equals(dto.getMinimumPercentageProfit(), product.getMinimumPercentageProfit())) {

            fieldAudits.add(auditForField(
                    product,
                    "minimumPercentageProfit",
                    product.getMinimumPercentageProfit() == null ? null : product.getMinimumPercentageProfit().toString(),
                    dto.getMinimumPercentageProfit().toString()
            ));

            product.setMinimumPercentageProfit(dto.getMinimumPercentageProfit());

            // Recompute all variant minimum selling price
            if (product.getVariants() != null) {
                for (ProductVariant savedVariant : product.getVariants()) {
                    BigDecimal newMinSelling = productVariantService.computeMinSelling(
                            savedVariant.getAverageBuyingPrice(),
                            dto.getMinimumPercentageProfit()
                    );
                    savedVariant.setMinimumSellingPrice(newMinSelling);
                    productVariantRepository.save(savedVariant);
                }
            }
        }

        Product updated = productRepository.save(product);

        // persist field audits
        if (!fieldAudits.isEmpty()) {
            productAuditRepository.saveAll(fieldAudits.stream().peek(a -> {
                a.setProductId(updated.getId());
                a.setProductName(updated.getName());
                a.setTimestamp(LocalDateTime.now());
                a.setPerformedBy(SecurityUtils.currentUsername());
            }).collect(Collectors.toList()));
        }

        return productMapper.toDTO(updated);
    }

    private void validateUpdateDTOUniqueness(ProductUpdateDTO dto) {

        // --- Name (unique)
        if (dto.getName() != null && productRepository.existsByName(dto.getName())) {
            throw new IllegalArgumentException("A product with the name '" + dto.getName() + "' already exists.");
        }
    }

    private void syncCategorySuppliers(Category category, Set<Supplier> productSuppliers) {
        if (productSuppliers == null || productSuppliers.isEmpty()) return;

        Set<Supplier> categorySuppliers = category.getSuppliers();

        boolean changed = false;
        for (Supplier supplier : productSuppliers) {
            if (!categorySuppliers.contains(supplier)) {
                categorySuppliers.add(supplier);
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
    public void uploadProductImages(UUID productId, List<MultipartFile> files) throws IOException {
        Product product = productRepository.findById(productId).orElseThrow(() -> new ProductNotFoundException("Product not found"));
        Path productDir = fileStorageService.initDirectory(productRoot().resolve(product.getId().toString()));
        saveProductImages(product, files);
        productRepository.save(product);
    }

    @Transactional
    public void saveProductImages(Product product, List<MultipartFile> files) throws IOException {
        if (files == null || files.isEmpty()) return;
        Path productDir = fileStorageService.initDirectory(productRoot().resolve(product.getId().toString()));
        fileStorageService.hidePathIfSupported(productDir);

        List<ProductImage> newImages = new ArrayList<>();
        for (var file : files) {
            if (file.isEmpty()) continue;
            String fileName = System.currentTimeMillis() + "_" + UUID.randomUUID() + "_" + sanitizeFilename(file.getOriginalFilename());
            try (InputStream in = file.getInputStream()) {
                Path saved = fileStorageService.saveFile(productDir, fileName, in);
                transactionalFileManager.track(saved); // for rollback cleanup
                fileStorageService.hidePath(saved);
                // store API-friendly path (not absolute)
                String apiPath = "/api/products/images/" + product.getId() + "/" + fileName;

                ProductImage pi = ProductImage.builder()
                        .fileName(fileName)
                        .filePath(apiPath)
                        .product(product)
                        .build();

                newImages.add(pi);
            }
        }

        productImageRepository.saveAll(newImages);
        if (product.getImages() == null) product.setImages(new ArrayList<>());
        product.getImages().addAll(newImages);

        // create image audit entries for each upload
        List<ProductImageAudit> audits = newImages.stream().map(img -> {
            ProductImageAudit ia = new ProductImageAudit();
            ia.setAction("IMAGE_UPLOADED");
            ia.setFileName(img.getFileName());
            ia.setFilePath(img.getFilePath());
            ia.setProductId(product.getId());
            ia.setProductName(product.getName());
            ia.setTimestamp(LocalDateTime.now());
            ia.setPerformedBy(SecurityUtils.currentUsername());
            return ia;
        }).collect(Collectors.toList());

        if (!audits.isEmpty()) productImageAuditRepository.saveAll(audits);
    }

    public void deleteProductImageByFilename(UUID productId, String filename) throws IOException {
        Product p = productRepository.findById(productId).orElseThrow(() -> new ProductNotFoundException("Product not found"));
        if (p.getImages() == null) return;
        for (ProductImage img : new ArrayList<>(p.getImages())) {
            if (filename.equals(img.getFileName())) {
                // delete physical file
                try {
                    // the stored filePath is API path; derive physical path
                    Path physical = productRoot().resolve(p.getId().toString()).resolve(img.getFileName());
                    Files.deleteIfExists(physical);
                } catch (Exception ignored) {}

                productImageRepository.delete(img);
                p.getImages().remove(img);

                // audit
                productImageAuditRepository.save(imageAuditForAction(p, img, "IMAGE_DELETED", "Deleted by user"));
            }
        }
        productRepository.save(p);
    }

    public void deleteAllProductImages(UUID productId) throws IOException {
        Product p = productRepository.findById(productId).orElseThrow(() -> new ProductNotFoundException("Product not found"));
        deleteProductImages(p);
        deleteProductUploadDirectory(productId);
        p.setImages(new ArrayList<>());
        productRepository.save(p);

        productImageAuditRepository.save(imageAuditForAction(p, null, "ALL_IMAGES_DELETED", "All images deleted"));
    }

    public void deleteProductImages(Product product) {
        if (product.getImages() == null) return;
            for (ProductImage img : new ArrayList<>(product.getImages())) {
                try {
                    ProductImageAudit audit = ProductImageAudit.builder()
                            .action("IMAGE_DELETED_PERMANENTLY")
                            .fileName(img.getFileName())
                            .filePath(img.getFilePath())
                            .productId(product.getId())
                            .productName(product.getName())
                            .timestamp(LocalDateTime.now())
                            .performedBy(SecurityUtils.currentUsername())
                            .build();
                    productImageAuditRepository.save(audit);

                    Path physical = productRoot().resolve(product.getId().toString()).resolve(img.getFileName());
                    Files.deleteIfExists(physical);
                } catch (IOException ignored) {}
                productImageRepository.delete(img);
            }
        }

    public void deleteProductUploadDirectory(UUID productId) throws IOException {
        Path dir = productRoot().resolve(productId.toString());
        fileStorageService.deleteDirectory(dir);
    }

    /* =============================
       SOFT / RESTORE / HARD DELETE (with audits)
       ============================= */

    @Transactional
    public ResponseEntity<ApiResponse> softDeleteProduct(UUID id) {
        Product p = productRepository.findById(id).orElseThrow(() -> new ProductNotFoundException("Product not found"));
        if (!p.getDeleted()) {
            p.setDeleted(true);
            p.setDeletedAt(LocalDateTime.now());
            productRepository.save(p);

            ProductAudit audit = new ProductAudit();
            audit.setAction("SOFT_DELETE");
            audit.setProductId(p.getId());
            audit.setProductName(p.getName());
            audit.setTimestamp(LocalDateTime.now());
            audit.setPerformedBy(SecurityUtils.currentUsername());
            productAuditRepository.save(audit);

            List<ProductImage> images = p.getImages();
            List<ProductImageAudit> imageAudits = new ArrayList<>();
            for(ProductImage image: images) {
                image.setDeleted(true);
                productImageRepository.save(image);

                ProductImageAudit imageAudit = ProductImageAudit.builder()
                        .action("IMAGE_SOFT_DELETED")
                        .fileName(image.getFileName())
                        .filePath(image.getFilePath())
                        .productId(p.getId())
                        .productName(p.getName())
                        .timestamp(LocalDateTime.now())
                        .performedBy(SecurityUtils.currentUsername())
                        .build();
                imageAudits.add(imageAudit);
            }
            productImageAuditRepository.saveAll(imageAudits);
            return ResponseEntity.ok(new ApiResponse("success", "Preduct soft-deleted successfully"));
        } else {
            return ResponseEntity.ok(new ApiResponse("success", "Product already soft-deleted"));
        }
    }

    @Transactional
    public void bulkSoftDelete(List<UUID> ids) {
        for (UUID id : ids) {
            Product p = productRepository.findById(id)
                    .orElseThrow(() -> new ProductNotFoundException("Product not found: " + id));
            p.setDeleted(true);
            p.setDeletedAt(LocalDateTime.now());
            productRepository.save(p);
        }
    }

    public void restoreProduct(UUID id) {
        Product p = productRepository.findById(id).orElseThrow(() -> new ProductNotFoundException("Product not found"));
        if (p.getDeleted()) {
            p.setDeleted(false);
            p.setDeletedAt(null);
            productRepository.save(p);

            ProductAudit audit = new ProductAudit();
            audit.setAction("RESTORE");
            audit.setProductId(p.getId());
            audit.setProductName(p.getName());
            audit.setTimestamp(LocalDateTime.now());
            audit.setPerformedBy(SecurityUtils.currentUsername());
            productAuditRepository.save(audit);
        }
    }

    @Transactional
    public void bulkRestore(List<UUID> ids) {
        for (UUID id : ids) {
            Product p = productRepository.findById(id)
                    .orElseThrow(() -> new ProductNotFoundException("Product not found: " + id));
            p.setDeleted(false);
            p.setDeletedAt(null);
            productRepository.save(p);
        }
    }

    @Transactional
    public void hardDeleteProduct(UUID id) throws IOException {
        Product p = productRepository.findById(id).orElseThrow(() -> new ProductNotFoundException("Product not found"));
        // audit before deletion
        ProductAudit audit = new ProductAudit();
        audit.setAction("HARD_DELETE");
        audit.setProductId(p.getId());
        audit.setProductName(p.getName());
        audit.setTimestamp(LocalDateTime.now());
        audit.setPerformedBy(SecurityUtils.currentUsername());
        productAuditRepository.save(audit);

        deleteProductImages(p);
        deleteProductUploadDirectory(p.getId());
        productRepository.delete(p);
    }

    @Transactional
    public void bulkHardDelete(List<UUID> ids) throws IOException {
        for (UUID id : ids) {
            hardDeleteProduct(id); // reuse existing single-delete logic
        }
    }




















    /* =============================
    PRODUCT IMAGES
   ============================= */
    public List<String> getProductImageUrls(UUID id, Boolean deleted) {
        Product product = productRepository.findById(id)
                .orElseThrow(() -> new ProductNotFoundException("Product not found"));

        if (product.getImages() == null) return List.of();

        return product.getImages()
                .stream()
                .filter(image ->
                        deleted == null
                                ? true                                       // return all
                                : deleted                                     // deleted = true → get deleted only
                                ? Boolean.TRUE.equals(image.getDeleted())
                                : Boolean.FALSE.equals(image.getDeleted()) // deleted = false → get active only
                )
                .map(ProductImage::getFilePath)
                .toList();
    }

    public File zipProductImages(UUID productId, Boolean deleted) throws IOException {
        Product product = productRepository.findById(productId)
                .orElseThrow(() -> new ProductNotFoundException("Product not found"));

        // Filter images based on deleted flag
        List<ProductImage> imagesToZip = product.getImages() == null
                ? List.of()
                : product.getImages().stream()
                .filter(image -> deleted == null
                        ? true                        // return all
                        : deleted                     // deleted = true → get deleted only
                        ? Boolean.TRUE.equals(image.getDeleted())
                        : Boolean.FALSE.equals(image.getDeleted()) // deleted = false → get active only
                )
                .toList();

        File zipFile = File.createTempFile("product-" + productId + "-images", ".zip");

        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(zipFile))) {
            for (ProductImage image : imagesToZip) {
                // Resolve the physical file path
                Path physicalPath = productRoot().resolve(product.getId().toString())
                        .resolve(Paths.get(image.getFilePath()).getFileName());

                if (Files.exists(physicalPath)) {
                    zos.putNextEntry(new ZipEntry(physicalPath.getFileName().toString()));
                    Files.copy(physicalPath, zos);
                    zos.closeEntry();
                }
            }
        }

        return zipFile;
    }

    public Map<UUID, List<String>> getAllProductImageUrls() {
        Map<UUID, List<String>> result = new HashMap<>();

        productRepository.findAll().forEach(product -> {
            List<String> imgs = product.getImages() == null
                    ? List.of()
                    : product.getImages().stream()
                    .map(ProductImage::getFilePath)
                    .toList();

            result.put(product.getId(), imgs);
        });

        return result;
    }

    public File zipAllProductImages(Boolean deletedProducts, Boolean deletedImages) throws IOException {
        File zipFile = File.createTempFile("all-product-images", ".zip");

        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(zipFile))) {
            // Filter products based on deletedProducts flag
            List<Product> products = productRepository.findAll().stream()
                    .filter(p -> deletedProducts == null
                            ? true
                            : deletedProducts
                            ? Boolean.TRUE.equals(p.getDeleted())
                            : Boolean.FALSE.equals(p.getDeleted())
                    )
                    .toList();

            for (Product product : products) {
                // Filter product images based on deletedImages flag
                List<ProductImage> imagesToZip = product.getImages() == null
                        ? List.of()
                        : product.getImages().stream()
                        .filter(img -> deletedImages == null
                                ? true
                                : deletedImages
                                ? Boolean.TRUE.equals(img.getDeleted())
                                : Boolean.FALSE.equals(img.getDeleted())
                        )
                        .toList();

                for (ProductImage image : imagesToZip) {
                    Path physicalPath = productRoot().resolve(product.getId().toString())
                            .resolve(Paths.get(image.getFilePath()).getFileName());

                    if (Files.exists(physicalPath)) {
                        String entryName = product.getId() + "/" + physicalPath.getFileName().toString();
                        zos.putNextEntry(new ZipEntry(entryName));
                        Files.copy(physicalPath, zos);
                        zos.closeEntry();
                    }
                }
            }
        }

        return zipFile;
    }








    /* =============================
       PRODUCT AUDITS
       ============================= */

    public List<ProductAudit> getProductAudits(UUID productId) {
        return productAuditRepository.findByProductIdOrderByTimestampDesc(productId);
    }

    public List<ProductImageAudit> getProductImagesAudits(UUID productId) {
        return productImageAuditRepository.findByProductId(productId)
                .stream()
                .sorted(Comparator.comparing(ProductImageAudit::getTimestamp).reversed())
                .toList();
    }

    public Map<UUID, List<ProductImageAudit>> getAllProductImagesAudits() {
        return productImageAuditRepository.findAll()
                .stream()
                .sorted(Comparator.comparing(ProductImageAudit::getTimestamp).reversed())
                .collect(Collectors.groupingBy(ProductImageAudit::getProductId));
    }












    /* =============================
       UTILITIES
       ============================= */

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

    private String sanitizeFilename(String n) {
        return (n == null) ? "file" : n.replaceAll("[^a-zA-Z0-9._-]", "_");
    }
}