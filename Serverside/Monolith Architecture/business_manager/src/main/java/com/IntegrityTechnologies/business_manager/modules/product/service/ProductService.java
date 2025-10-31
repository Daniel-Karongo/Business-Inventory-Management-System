package com.IntegrityTechnologies.business_manager.modules.product.service;

import com.IntegrityTechnologies.business_manager.exception.*;
import com.IntegrityTechnologies.business_manager.modules.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.category.repository.CategoryRepository;
import com.IntegrityTechnologies.business_manager.config.FileStorageProperties;
import com.IntegrityTechnologies.business_manager.modules.product.dto.ProductDTO;
import com.IntegrityTechnologies.business_manager.modules.product.mapper.ProductMapper;
import com.IntegrityTechnologies.business_manager.modules.product.model.Product;
import com.IntegrityTechnologies.business_manager.modules.product.model.ProductImage;
import com.IntegrityTechnologies.business_manager.modules.product.repository.ProductImageRepository;
import com.IntegrityTechnologies.business_manager.modules.product.repository.ProductRepository;
import com.IntegrityTechnologies.business_manager.modules.product.specification.ProductSpecification;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.*;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.*;
import java.util.ArrayList;
import java.util.List;

@Service
@RequiredArgsConstructor
public class ProductService {

    private final ProductRepository productRepository;
    private final CategoryRepository categoryRepository;
    private final ProductImageRepository productImageRepository;
    private final FileStorageProperties fileStorageProperties;
    private final ProductMapper productMapper;

    // Save or update product
    public ProductDTO saveProduct(ProductDTO dto) throws IOException {
        Product product = dto.getId() != null
                ? productRepository.findById(dto.getId()).orElse(new Product())
                : new Product();

        // Map fields from DTO to entity (MapStruct)
        productMapper.updateEntityFromDTO(dto, product);

        // Ensure category exists
        if (dto.getCategoryId() != null) {
            Category category = categoryRepository.findById(dto.getCategoryId())
                    .orElseThrow(() -> new CategoryNotFoundException("Category not found with ID: " + dto.getCategoryId()));
            product.setCategory(category);
        }

        // Persist product first so images can reference it
        product = productRepository.save(product);

        // Save uploaded images (support multiple)
        if (dto.getImageFiles() != null && !dto.getImageFiles().isEmpty()) {
            saveProductImages(dto.getImageFiles(), product);
        }

        // Build response DTO and include image URLs
        return toResponseDTO(product);
    }

    private void saveProductImages(List<MultipartFile> imageFiles, Product product) throws IOException {
        List<ProductImage> newImages = new ArrayList<>();
        Path uploadPath = Paths.get(fileStorageProperties.getProductUploadDir()).toAbsolutePath().normalize();

        // ensure path exists
        if (!Files.exists(uploadPath)) Files.createDirectories(uploadPath);

        for (MultipartFile file : imageFiles) {
            if (file.isEmpty()) continue;

            String fileName = System.currentTimeMillis() + "_" + file.getOriginalFilename();
            Path filePath = uploadPath.resolve(fileName);

            try {
                file.transferTo(filePath);
            } catch (IOException e) {
                // try to detect low-disk space
                String msg = e.getMessage() == null ? "" : e.getMessage().toLowerCase();
                if (msg.contains("no space") || msg.contains("disk full") || msg.contains("not enough space")) {
                    throw new StorageFullException("Storage is full. Unable to upload image: " + file.getOriginalFilename());
                }
                throw e;
            }

            ProductImage image = ProductImage.builder()
                    .fileName(fileName)
                    .filePath(filePath.toString())
                    .product(product)
                    .build();
            newImages.add(image);
        }

        productImageRepository.saveAll(newImages);

        // attach to product entity in memory for immediate response
        if (product.getImages() == null) product.setImages(new ArrayList<>());
        product.getImages().addAll(newImages);
    }

    // Paginated & filtered products with optional includeDeleted flag
    public Page<ProductDTO> getProductsAdvanced(
            List<Long> categoryIds,
            String name,
            String description,
            BigDecimal minPrice,
            BigDecimal maxPrice,
            int page,
            int size,
            String sortBy,
            String direction,
            boolean includeDeleted
    ) {
        Sort sort = direction.equalsIgnoreCase("desc") ? Sort.by(sortBy).descending() : Sort.by(sortBy).ascending();
        Pageable pageable = PageRequest.of(page, size, sort);

        Specification<Product> spec = ProductSpecification.filterProducts(categoryIds, name, description, minPrice, maxPrice);

        if (!includeDeleted) {
            spec = spec.and((root, query, cb) -> cb.isFalse(root.get("deleted")));
        }

        Page<Product> products = productRepository.findAll(spec, pageable);
        return products.map(this::toResponseDTO);
    }

    // Active / Deleted lists
    public List<ProductDTO> getActiveProducts() {
        return productRepository.findByDeletedFalse().stream().map(this::toResponseDTO).toList();
    }

    public List<ProductDTO> getDeletedProducts() {
        return productRepository.findByDeletedTrue().stream().map(this::toResponseDTO).toList();
    }

    // Get single product (includeDeleted flag)
    public ProductDTO getProduct(Long id, boolean includeDeleted) {
        Product product = productRepository.findById(id)
                .filter(p -> includeDeleted || !p.isDeleted())
                .orElseThrow(() -> new ProductNotFoundException("Product not found with ID: " + id));

        return toResponseDTO(product);
    }

    // Soft delete
    public void softDeleteProduct(Long id) {
        Product product = productRepository.findById(id)
                .orElseThrow(() -> new ProductNotFoundException("Product not found with ID: " + id));
        product.setDeleted(true);
        productRepository.save(product);
    }

    // Restore soft-deleted
    public void restoreProduct(Long id) {
        Product product = productRepository.findById(id)
                .orElseThrow(() -> new ProductNotFoundException("Product not found with ID: " + id));
        if (!product.isDeleted()) {
            // idempotent - nothing to do
            return;
        }
        product.setDeleted(false);
        product.setDeletedAt(null);
        productRepository.save(product);
    }

    // Permanent delete (delete DB record and image files)
    public void permanentDeleteProduct(Long id) throws IOException {
        Product product = productRepository.findById(id)
                .orElseThrow(() -> new ProductNotFoundException("Product not found with ID: " + id));

        // Delete image files from disk
        if (product.getImages() != null) {
            for (ProductImage image : product.getImages()) {
                try {
                    Files.deleteIfExists(Paths.get(image.getFilePath()));
                } catch (IOException e) {
                    // if we cannot delete a file, bubble up or log — here we bubble up
                    throw new IOException("Failed to remove image " + image.getFileName() + ": " + e.getMessage(), e);
                }
            }
        }

        // Remove image records then product
        productImageRepository.deleteAll(product.getImages() != null ? product.getImages() : List.of());
        productRepository.delete(product);
    }

    // Serve image
    public Resource loadProductImage(String filename) throws IOException {
        Path imagePath = Paths.get(fileStorageProperties.getProductUploadDir()).resolve(filename).normalize();
        File file = imagePath.toFile();

        if (!file.exists()) {
            throw new FileNotFoundException("Image not found: " + filename);
        }

        return new FileSystemResource(file);
    }

    // Map entity -> DTO and set image URL list
    private ProductDTO toResponseDTO(Product product) {
        ProductDTO dto = productMapper.toDTO(product);

        // Build image URLs for API consumers. Keep path stable with controller route.
        if (product.getImages() != null) {
            dto.setImageUrls(product.getImages().stream()
                    .map(img -> "/api/products/image/" + img.getFileName())
                    .toList());
        }

        return dto;
    }
}