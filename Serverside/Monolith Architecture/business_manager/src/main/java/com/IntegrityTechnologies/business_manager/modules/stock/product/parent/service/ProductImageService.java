package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service;

import com.IntegrityTechnologies.business_manager.config.files.FileStorageService;
import com.IntegrityTechnologies.business_manager.config.files.TransactionalFileManager;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.FileAssignmentDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductImage;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductImageAudit;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductImageAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductImageRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.service.ProductVariantImageService;
import com.IntegrityTechnologies.business_manager.security.util.SecurityUtils;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import net.coobird.thumbnailator.Thumbnails;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class ProductImageService {

    private final ProductImageRepository productImageRepository;
    private final ProductVariantImageService productVariantImageService;
    private final FileStorageService fileStorageService;
    private final TransactionalFileManager transactionalFileManager;
    private final ProductImageAuditRepository productImageAuditRepository;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    private void assertOwnership(UUID branchId, Product product) {
        if (!tenantId().equals(product.getTenantId())
                || !branchId.equals(product.getBranchId())) {
            throw new IllegalStateException("Cross-tenant/branch access detected");
        }
    }

    @Transactional
    public void attachFilesWithAssignments(
            UUID branchId,
            Product product,
            List<ProductVariant> variants,
            List<FileAssignmentDTO> assignments,
            List<MultipartFile> files
    ) throws IOException {

        if (files == null || files.isEmpty()) return;

        assertOwnership(branchId, product);

        Map<String, MultipartFile> fileMap =
                files.stream().collect(Collectors.toMap(
                        MultipartFile::getOriginalFilename,
                        f -> f
                ));

        for (FileAssignmentDTO assignment : assignments) {

            MultipartFile file = fileMap.get(assignment.getFileName());
            if (file == null) continue;

            if (Boolean.TRUE.equals(assignment.getAssignToProduct())) {
                saveProductImages(branchId, product, List.of(file));
            }

            if (assignment.getVariantClassifications() != null) {

                for (String classification : assignment.getVariantClassifications()) {

                    ProductVariant variant =
                            variants.stream()
                                    .filter(v -> v.getClassification().equalsIgnoreCase(classification))
                                    .findFirst()
                                    .orElse(null);

                    if (variant != null) {
                        productVariantImageService.saveVariantImage(variant.getBranchId(), variant, file);
                    }
                }
            }
        }
    }

    @Transactional
    public void saveProductImages(UUID branchId, Product product, List<MultipartFile> files) throws IOException {

        if (files == null || files.isEmpty()) return;

        assertOwnership(branchId, product);

        UUID tenantId = tenantId();

        Path sharedDir = fileStorageService.initDirectory(
                fileStorageService.productSharedRoot(branchId)
        );

        List<ProductImage> newImages = new ArrayList<>();

        boolean hasPrimaryAlready =
                product.getImages() != null &&
                        product.getImages().stream()
                                .anyMatch(img -> Boolean.TRUE.equals(img.getPrimaryImage())
                                        && !Boolean.TRUE.equals(img.isDeleted()));

        for (MultipartFile file : files) {

            validateFile(file);

            String hash = computeHash(file);

            Optional<ProductImage> existing =
                    productImageRepository
                            .findFirstByTenantIdAndBranchIdAndContentHash(
                                    tenantId,
                                    product.getBranchId(),
                                    hash
                            );

            String extension = getExtension(file.getOriginalFilename());
            String fileName;
            String thumbnailName;

            if (existing.isPresent()) {

                ProductImage existingImage = existing.get();
                fileName = existingImage.getFileName();
                thumbnailName = existingImage.getThumbnailFileName();

            } else {

                fileName = hash + extension;
                thumbnailName = "thumb_" + hash + ".jpg";

                Path saved = sharedDir.resolve(fileName);
                Path thumbnailPath = sharedDir.resolve(thumbnailName);

                if (!Files.exists(saved)) {

                    try (InputStream in = file.getInputStream()) {

                        saved = fileStorageService.saveFile(sharedDir, fileName, in);
                        transactionalFileManager.track(saved);

                        thumbnailPath = createThumbnail(sharedDir, fileName, saved);
                        transactionalFileManager.track(thumbnailPath);
                    }
                }
            }

            boolean makePrimary = !hasPrimaryAlready;
            hasPrimaryAlready = true;

            ProductImage pi = ProductImage.builder()
                    .fileName(fileName)
                    .thumbnailFileName(thumbnailName)
                    .filePath("/api/products/images/shared/" + fileName)
                    .product(product)
                    .contentHash(hash)
                    .primaryImage(makePrimary)
                    .deleted(false)
                    .deletedIndependently(false)
                    .tenantId(tenantId)
                    .branchId(product.getBranchId())
                    .build();

            newImages.add(pi);
        }

        if (!newImages.isEmpty()) {

            productImageRepository.saveAll(newImages);

            if (product.getImages() == null) {
                product.setImages(new HashSet<>());
            }

            product.getImages().addAll(newImages);

            productImageAuditRepository.saveAll(
                    newImages.stream()
                            .map(img -> imageAudit(product, img, "IMAGE_LINKED_OR_UPLOADED"))
                            .toList()
            );
        }
    }

    @Transactional(readOnly = true)
    public ResponseEntity<Resource> getSharedImage(
            UUID branchId,
            String fileName
    ) {

        Path path =
                fileStorageService
                        .productSharedRoot(branchId)
                        .resolve(fileName);

        if (!Files.exists(path)) {
            return ResponseEntity.notFound().build();
        }

        Resource resource =
                new FileSystemResource(
                        path.toFile()
                );

        MediaType mediaType =
                MediaType.APPLICATION_OCTET_STREAM;

        try {

            String etag =
                    "\"" +
                            Files.getLastModifiedTime(path)
                                    .toMillis()
                            + "\"";

            String ifNoneMatch =
                    RequestContextHolder
                            .currentRequestAttributes()
                            instanceof ServletRequestAttributes attrs
                            ? attrs.getRequest()
                            .getHeader(HttpHeaders.IF_NONE_MATCH)
                            : null;

            if (etag.equals(ifNoneMatch)) {
                return ResponseEntity
                        .status(HttpStatus.NOT_MODIFIED)
                        .eTag(etag)
                        .build();
            }

            return ResponseEntity.ok()
                    .eTag(etag)
                    .contentType(mediaType)
                    .header(
                            HttpHeaders.CACHE_CONTROL,
                            "public, max-age=86400"
                    )
                    .body(resource);

        } catch (IOException e) {
            throw new RuntimeException(
                    "Failed to read image",
                    e
            );
        }
    }

    private String getExtension(String filename) {
        if (filename == null || !filename.contains(".")) return ".bin";
        return filename.substring(filename.lastIndexOf("."));
    }

    private Path createThumbnail(Path productDir, String fileName, Path source) throws IOException {

        String baseName = fileName.replaceAll("\\.[^.]+$", "");
        String thumbName = "thumb_" + baseName + ".jpg";

        Path thumbPath = productDir.resolve(thumbName);

        Thumbnails.of(source.toFile())
                .size(300, 300)
                .outputFormat("jpg")
                .toFile(thumbPath.toFile());

        fileStorageService.secure(thumbPath);

        return thumbPath;
    }

    private void validateFile(MultipartFile file) {

        if (file == null || file.isEmpty()) {
            throw new IllegalArgumentException("File is empty");
        }

        String name = file.getOriginalFilename();

        if (name == null || name.isBlank()) {
            throw new IllegalArgumentException("Invalid file name");
        }

        if (file.getSize() > 10 * 1024 * 1024) {
            throw new IllegalArgumentException("File exceeds 10MB limit");
        }
    }

    private ProductImageAudit imageAudit(
            Product product,
            ProductImage img,
            String action
    ) {
        ProductImageAudit ia =new ProductImageAudit();

        ia.setTenantId(product.getTenantId());
        ia.setBranchId(product.getBranchId());
        ia.setAction(action);
        ia.setFileName(img.getFileName());
        ia.setFilePath(img.getFilePath());
        ia.setProductId(product.getId());
        ia.setProductName(product.getName());
        ia.setTimestamp(LocalDateTime.now());
        ia.setPerformedBy(SecurityUtils.currentUsername());
        return ia;
    }

    private String computeHash(MultipartFile file) {
        try (InputStream is = file.getInputStream()) {

            MessageDigest digest = MessageDigest.getInstance("SHA-256");

            byte[] buffer = new byte[8192];
            int read;

            while ((read = is.read(buffer)) != -1) {
                digest.update(buffer, 0, read);
            }

            return HexFormat.of().formatHex(digest.digest());

        } catch (Exception e) {
            throw new RuntimeException("Failed to compute file hash", e);
        }
    }
}