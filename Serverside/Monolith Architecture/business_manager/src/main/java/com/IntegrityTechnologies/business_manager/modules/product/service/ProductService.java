package com.IntegrityTechnologies.business_manager.modules.product.service;

import com.IntegrityTechnologies.business_manager.config.FileStorageProperties;
import com.IntegrityTechnologies.business_manager.config.FileStorageService;
import com.IntegrityTechnologies.business_manager.exception.ProductNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.product.dto.ProductDTO;
import com.IntegrityTechnologies.business_manager.modules.product.mapper.ProductMapper;
import com.IntegrityTechnologies.business_manager.modules.product.model.Product;
import com.IntegrityTechnologies.business_manager.modules.product.model.ProductImage;
import com.IntegrityTechnologies.business_manager.modules.product.repository.ProductImageRepository;
import com.IntegrityTechnologies.business_manager.modules.product.repository.ProductRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.*;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.io.*;
import java.math.BigDecimal;
import java.nio.file.*;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * ProductService – full feature implementation.
 *
 * Delegates barcode image/PDF generation to BarcodeService and all filesystem ops to FileStorageService.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class ProductService {

    private final ProductRepository productRepository;
    private final ProductImageRepository productImageRepository;
    private final ProductMapper productMapper;
    private final FileStorageProperties fileStorageProperties;
    private final FileStorageService fileStorageService;
    private final BarcodeService barcodeService;

    private Path productRoot() {
        return Paths.get(fileStorageProperties.getProductUploadDir()).toAbsolutePath().normalize();
    }

    /* =============================
       BASIC READ / FILTER
       ============================= */
    public List<ProductDTO> getAllActiveProducts() {
        return productRepository.findByDeletedFalse().stream()
                .map(productMapper::toDTO)
                .collect(Collectors.toList());
    }

    public List<ProductDTO> getAllDeletedProducts() {
        return productRepository.findByDeletedTrue().stream()
                .map(productMapper::toDTO)
                .collect(Collectors.toList());
    }

    public List<ProductDTO> getAllActiveAndDeletedProducts() {
        return productRepository.findAll().stream()
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

    public ProductDTO getProductById(Long id, boolean includeDeleted) {
        Product p = productRepository.findById(id)
                .filter(prod -> includeDeleted || !prod.isDeleted())
                .orElseThrow(() -> new ProductNotFoundException("Product not found: " + id));
        return productMapper.toDTO(p);
    }

    public ProductDTO getProductByBarcode(String barcode) {
        return productRepository.findByBarcode(barcode)
                .map(productMapper::toDTO)
                .orElseThrow(() -> new ProductNotFoundException("Product with barcode not found: " + barcode));
    }

    /* =============================
       CREATE / UPDATE
       ============================= */
    @Transactional
    public ProductDTO createProduct(ProductDTO dto, List<MultipartFile> imageFiles) throws IOException {
        Product product = productMapper.toEntity(dto);

        // Ensure barcode exists / generate if missing
        if (product.getBarcode() == null || product.getBarcode().isBlank()) {
            product.setBarcode(barcodeService.autoGenerateBarcode());
        } else if (!barcodeService.isValidBarcode(product.getBarcode())) {
            throw new IllegalArgumentException("Provided barcode has invalid format");
        }

        // persist to get ID (so images dir uses id)
        product = productRepository.save(product);

        // generate barcode image
        Path barcodeDir = productRoot().resolve("barcodes");
        fileStorageService.initDirectory(barcodeDir);
        String barcodeImagePath = barcodeService.generateBarcodeImage(product.getBarcode(), barcodeDir);
        product.setBarcodeImagePath(barcodeImagePath);

        // save product images if provided
        if (imageFiles != null && !imageFiles.isEmpty()) {
            saveProductImages(product, imageFiles);
        }

        product = productRepository.save(product);
        log.info("Created product {} (id={} barcode={})", product.getName(), product.getId(), product.getBarcode());
        return productMapper.toDTO(product);
    }

    @Transactional
    public ProductDTO updateProduct(Long id, ProductDTO dto, List<MultipartFile> imageFiles) throws IOException {
        Product product = productRepository.findById(id)
                .orElseThrow(() -> new ProductNotFoundException("Product not found: " + id));

        // partial field updates
        if (dto.getName() != null && !dto.getName().isBlank()) product.setName(dto.getName());
        if (dto.getDescription() != null) product.setDescription(dto.getDescription());
        if (dto.getPrice() != null) product.setPrice(dto.getPrice());
        if (dto.getBuyingPrice() != null) product.setBuyingPrice(dto.getBuyingPrice());

        if (dto.getSku() != null && !dto.getSku().isBlank() && !dto.getSku().equals(product.getSku())) {
            if (productRepository.existsBySku(dto.getSku())) throw new IllegalArgumentException("SKU already exists");
            product.setSku(dto.getSku());
        }

        if (dto.getBarcodes() != null && !dto.getBarcodes().isEmpty()) {
            String newCode = dto.getBarcodes().get(0);
            if (!newCode.equals(product.getBarcode())) {
                if (productRepository.existsByBarcode(newCode))
                    throw new IllegalArgumentException("Barcode already exists");
                if (!barcodeService.isValidBarcode(newCode))
                    throw new IllegalArgumentException("Provided barcode has invalid format");
                product.setBarcode(newCode);

                Path barcodeDir = productRoot().resolve("barcodes");
                fileStorageService.initDirectory(barcodeDir);
                String path = barcodeService.generateBarcodeImage(newCode, barcodeDir);
                product.setBarcodeImagePath(path);
            }
        }

        // Keep supplierId/categoryId handling to mapper (module integration later)

        product.setUpdatedAt(LocalDateTime.now());

        // replace product images if new files provided (null => keep existing; empty list => clear)
        if (imageFiles != null) {
            // wipe existing files and DB rows
            deleteProductUploadDirectory(product.getId());
            if (!imageFiles.isEmpty()) {
                saveProductImages(product, imageFiles);
            } else {
                product.setImages(new ArrayList<>());
            }
        }

        product = productRepository.save(product);
        return productMapper.toDTO(product);
    }

    /* =============================
       SOFT / RESTORE / HARD DELETE
       ============================= */
    public void softDeleteProduct(Long id) {
        Product p = productRepository.findById(id).orElseThrow(() -> new ProductNotFoundException("Product not found"));
        if (!p.isDeleted()) {
            p.setDeleted(true);
            p.setDeletedAt(LocalDateTime.now());
            productRepository.save(p);
        }
    }

    public void restoreProduct(Long id) {
        Product p = productRepository.findById(id).orElseThrow(() -> new ProductNotFoundException("Product not found"));
        if (p.isDeleted()) {
            p.setDeleted(false);
            p.setDeletedAt(null);
            productRepository.save(p);
        }
    }

    @Transactional
    public void hardDeleteProduct(Long id) throws IOException {
        Product p = productRepository.findById(id).orElseThrow(() -> new ProductNotFoundException("Product not found"));
        deleteProductImages(p);
        deleteProductUploadDirectory(p.getId());
        productRepository.delete(p);
    }

    /* =============================
       IMAGE HANDLING
       ============================= */
    @Transactional
    public void uploadProductImages(Long productId, List<MultipartFile> files) throws IOException {
        Product product = productRepository.findById(productId).orElseThrow(() -> new ProductNotFoundException("Product not found"));
        Path productDir = fileStorageService.initDirectory(productRoot().resolve(product.getId().toString()));
        saveProductImages(product, files);
        productRepository.save(product);
    }

    @Transactional
    public void saveProductImages(Product product, List<MultipartFile> files) throws IOException {
        if (files == null || files.isEmpty()) return;
        Path productDir = fileStorageService.initDirectory(productRoot().resolve(product.getId().toString()));

        List<ProductImage> newImages = new ArrayList<>();
        for (var file : files) {
            if (file.isEmpty()) continue;
            String fileName = System.currentTimeMillis() + "_" + UUID.randomUUID() + "_" + sanitizeFilename(file.getOriginalFilename());
            try (InputStream in = file.getInputStream()) {
                Path saved = fileStorageService.saveFile(productDir, fileName, in);
                newImages.add(ProductImage.builder()
                        .fileName(fileName)
                        .filePath(saved.toString())
                        .product(product)
                        .build());
            }
        }
        productImageRepository.saveAll(newImages);
        if (product.getImages() == null) product.setImages(new ArrayList<>());
        product.getImages().addAll(newImages);
    }

    public void deleteProductImageByFilename(Long productId, String filename) throws IOException {
        Product p = productRepository.findById(productId).orElseThrow(() -> new ProductNotFoundException("Product not found"));
        if (p.getImages() == null) return;
        for (ProductImage img : new ArrayList<>(p.getImages())) {
            if (filename.equals(img.getFileName())) {
                try { Files.deleteIfExists(Paths.get(img.getFilePath())); } catch (IOException ignored) {}
                productImageRepository.delete(img);
                p.getImages().remove(img);
            }
        }
        productRepository.save(p);
    }

    public void deleteAllProductImages(Long productId) throws IOException {
        Product p = productRepository.findById(productId).orElseThrow(() -> new ProductNotFoundException("Product not found"));
        deleteProductImages(p);
        deleteProductUploadDirectory(productId);
        p.setImages(new ArrayList<>());
        productRepository.save(p);
    }

    public void deleteProductImages(Product product) {
        if (product.getImages() == null) return;
        for (ProductImage img : new ArrayList<>(product.getImages())) {
            try { Files.deleteIfExists(Paths.get(img.getFilePath())); } catch (IOException ignored) {}
            productImageRepository.delete(img);
        }
    }

    public void deleteProductUploadDirectory(Long productId) throws IOException {
        Path dir = productRoot().resolve(productId.toString());
        fileStorageService.deleteDirectory(dir);
    }

    /* =============================
       BARCODE & LABEL PDF
       ============================= */
    public File getBarcodeImageFile(String barcode) {
        Product product = productRepository.findByBarcode(barcode)
                .orElseThrow(() -> new ProductNotFoundException("Product not found"));
        return (product.getBarcodeImagePath() == null) ? null : new File(product.getBarcodeImagePath());
    }

    public File getBarcodePdfSingle(String barcode) throws IOException {
        Product p = productRepository.findByBarcode(barcode).orElseThrow(() -> new ProductNotFoundException("Product not found"));

        if (p.getBarcodeImagePath() == null || !Files.exists(Paths.get(p.getBarcodeImagePath()))) {
            Path barcodeDir = productRoot().resolve("barcodes");
            fileStorageService.initDirectory(barcodeDir);
            String path = barcodeService.generateBarcodeImage(p.getBarcode(), barcodeDir);
            p.setBarcodeImagePath(path);
            productRepository.save(p);
        }

        // Let BarcodeService produce the polished PDF (returns path)
        String pdfPath = barcodeService.generateBarcodeLabelPDF(p.getBarcode(), p.getName(), productRoot().resolve("labels"));
        return new File(pdfPath);
    }

    public File getBarcodePdfSheet(List<Long> ids) throws IOException {
        List<Product> products = productRepository.findAllById(ids);
        if (products.isEmpty()) throw new ProductNotFoundException("No products found");

        // Ensure barcode images exist and then delegate sheet creation to BarcodeService
        Path barcodeDir = productRoot().resolve("barcodes");
        fileStorageService.initDirectory(barcodeDir);

        for (Product p : products) {
            if (p.getBarcodeImagePath() == null || !Files.exists(Paths.get(p.getBarcodeImagePath()))) {
                String path = barcodeService.generateBarcodeImage(p.getBarcode(), barcodeDir);
                p.setBarcodeImagePath(path);
                productRepository.save(p);
            }
        }

        String sheetPath = barcodeService.generateBarcodeSheetPDF(products, productRoot().resolve("labels"));
        return new File(sheetPath);
    }

    /* =============================
       UTILITIES
       ============================= */
    private String sanitizeFilename(String n) {
        return (n == null) ? "file" : n.replaceAll("[^a-zA-Z0-9._-]", "_");
    }
}