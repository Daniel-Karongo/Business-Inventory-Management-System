package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service;

import com.IntegrityTechnologies.business_manager.config.FileStorageProperties;
import com.IntegrityTechnologies.business_manager.config.FileStorageService;
import com.IntegrityTechnologies.business_manager.config.TransactionalFileManager;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.FileAssignmentDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductImage;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductImageAudit;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductImageAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductImageRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.service.ProductVariantImageService;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import net.coobird.thumbnailator.Thumbnails;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class ProductImageService {

    private final ProductImageRepository productImageRepository;
    private final ProductVariantImageService productVariantImageService;
    private final FileStorageService fileStorageService;
    private final TransactionalFileManager transactionalFileManager;
    private final ProductImageAuditRepository productImageAuditRepository;

    private Path productRoot() {
        return fileStorageService.productRoot();
    }

    @Transactional
    public void attachFilesWithAssignments(
            Product product,
            List<ProductVariant> variants,
            List<FileAssignmentDTO> assignments,
            List<MultipartFile> files
    ) throws IOException {

        if (files == null || files.isEmpty()) return;

        Map<String, MultipartFile> fileMap =
                files.stream().collect(Collectors.toMap(
                        MultipartFile::getOriginalFilename,
                        f -> f
                ));

        for (FileAssignmentDTO assignment : assignments) {

            MultipartFile file = fileMap.get(assignment.getFileName());
            if (file == null) continue;

            if (Boolean.TRUE.equals(assignment.getAssignToProduct())) {

                saveProductImages(product, List.of(file));
            }

            if (assignment.getVariantClassifications() != null) {

                for (String classification : assignment.getVariantClassifications()) {

                    ProductVariant variant =
                            variants.stream()
                                    .filter(v -> v.getClassification().equalsIgnoreCase(classification))
                                    .findFirst()
                                    .orElse(null);

                    if (variant != null) {

                        productVariantImageService.saveVariantImage(variant, file);
                    }
                }
            }
        }
    }

    @Transactional
    public void saveProductImages(Product product, List<MultipartFile> files) throws IOException {

        if (files == null || files.isEmpty()) return;

        Path productDir = fileStorageService.initDirectory(
                productRoot().resolve(product.getId().toString())
        );

        List<ProductImage> newImages = new ArrayList<>();

        boolean hasPrimaryAlready =
                product.getImages() != null &&
                        product.getImages().stream()
                                .anyMatch(img -> Boolean.TRUE.equals(img.getPrimaryImage())
                                        && !Boolean.TRUE.equals(img.getDeleted()));

        for (MultipartFile file : files) {

            if (file.isEmpty()) continue;

            // ==============================
            // 1️⃣ Generate safe file name
            // ==============================

            String sanitized =
                    file.getOriginalFilename()
                            .replaceAll("[^a-zA-Z0-9._-]", "_");

            String fileName =
                    System.currentTimeMillis() + "_"
                            + UUID.randomUUID() + "_"
                            + sanitized;

            // ==============================
            // 2️⃣ Save original image
            // ==============================

            Path saved;

            try (InputStream in = file.getInputStream()) {
                saved = fileStorageService.saveFile(productDir, fileName, in);
                transactionalFileManager.track(saved);
            }

            // ==============================
            // 3️⃣ Generate thumbnail
            // ==============================

            String baseName = fileName.replaceAll("\\.[^.]+$", "");
            String thumbnailFileName = "thumb_" + baseName + ".jpg";
            Path thumbnailPath = productDir.resolve(thumbnailFileName);

            Thumbnails.of(saved.toFile())
                    .size(300, 300)
                    .outputFormat("jpg")
                    .toFile(thumbnailPath.toFile());

            transactionalFileManager.track(thumbnailPath);

            // 🔐 Secure thumbnail (since it was not created via saveFile)
            fileStorageService.secure(thumbnailPath);

            // ==============================
            // 4️⃣ Determine primary image
            // ==============================

            boolean makePrimary = !hasPrimaryAlready;
            hasPrimaryAlready = true; // after first

            // ==============================
            // 5️⃣ Build entity
            // ==============================

            ProductImage pi = ProductImage.builder()
                    .fileName(fileName)
                    .thumbnailFileName(thumbnailFileName)
                    .filePath("/api/products/images/" + product.getId() + "/" + fileName)
                    .product(product)
                    .primaryImage(makePrimary)
                    .deleted(false)
                    .build();

            newImages.add(pi);
        }

        if (!newImages.isEmpty()) {

            productImageRepository.saveAll(newImages);

            if (product.getImages() == null) {
                product.setImages(new ArrayList<>());
            }

            product.getImages().addAll(newImages);

            // ==============================
            // 6️⃣ Audit entries
            // ==============================

            List<ProductImageAudit> audits =
                    newImages.stream().map(img -> {

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

            productImageAuditRepository.saveAll(audits);
        }
    }
}