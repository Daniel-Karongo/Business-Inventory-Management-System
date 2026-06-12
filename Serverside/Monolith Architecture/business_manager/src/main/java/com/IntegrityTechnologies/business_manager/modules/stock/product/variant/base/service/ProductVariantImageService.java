package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.service;

import com.IntegrityTechnologies.business_manager.config.files.FileStorageService;
import com.IntegrityTechnologies.business_manager.config.files.TransactionalFileManager;
import com.IntegrityTechnologies.business_manager.config.kafka.OutboxEventWriter;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.VariantImageAuditDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.VariantImageDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.events.VariantImageUploadRequestedEvent;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariantImage;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariantImageAudit;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.repository.ProductVariantImageAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.repository.ProductVariantImageRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.repository.ProductVariantRepository;
import com.IntegrityTechnologies.business_manager.security.util.SecurityUtils;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import net.coobird.thumbnailator.Thumbnails;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.server.ResponseStatusException;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.time.LocalDateTime;
import java.util.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import static org.aspectj.weaver.tools.cache.SimpleCacheFactory.path;

@Service
@RequiredArgsConstructor
public class ProductVariantImageService {

    private final ProductVariantRepository variantRepo;
    private final ProductVariantImageRepository imageRepo;
    private final FileStorageService fileStorageService;
    private final TransactionalFileManager transactionalFileManager;
    private final OutboxEventWriter outboxEventWriter;
    private final ProductVariantImageAuditRepository auditRepo;

    private UUID tenantId() { return TenantContext.getTenantId(); }

    private ProductVariant getVariant(
            UUID branchId,
            UUID variantId
    ) {
        return variantRepo.findByIdIncludingDeleted(
                variantId,
                tenantId(),
                branchId
        ).orElseThrow(
                () -> new EntityNotFoundException(
                        "Variant not found"
                )
        );
    }

    private Path sharedDir(UUID branchId) {
        return fileStorageService.initDirectory(
                fileStorageService.productSharedRoot(branchId).resolve("_variant")
        );
    }

    /* ============================================================
       UPLOAD
       ============================================================ */

    @Transactional
    public void uploadVariantImages(UUID branchId, UUID variantId, List<MultipartFile> files) throws IOException {

        if (files == null || files.isEmpty()) return;

        for (MultipartFile file : files) {

            validateFile(file);

            Path tempDir = fileStorageService.initDirectory(
                    fileStorageService.productSharedRoot(branchId).resolve("_tmp")
            );

            String tempName = UUID.randomUUID() + "_" + file.getOriginalFilename();

            Path tempFile = tempDir.resolve(tempName);

            try (InputStream in = file.getInputStream()) {
                fileStorageService.saveFile(
                        tempDir,
                        tempName,
                        in
                );
            }

            outboxEventWriter.write(
                    "VARIANT_IMAGE_UPLOAD_REQUESTED",
                    branchId,
                    VariantImageUploadRequestedEvent.builder()
                            .uploadId(UUID.randomUUID())
                            .variantId(variantId)
                            .tenantId(tenantId())
                            .branchId(branchId)
                            .fileName(file.getOriginalFilename())
                            .contentType(file.getContentType())
                            .tempFilePath(tempFile.toString())
                            .build()
            );
        }
    }

    @Transactional
    public void saveVariantImage(UUID branchId, ProductVariant variant, MultipartFile file) throws IOException {

        if (file == null || file.isEmpty()) return;

        validateFile(file);

        String hash = computeHash(file);

        Optional<ProductVariantImage> existing =
                imageRepo.findFirstByTenantIdAndBranchIdAndContentHash(
                        tenantId(),
                        branchId,
                        hash
                );

        String extension = getExtension(file.getOriginalFilename());
        String fileName;
        String thumbnailName;

        if (existing.isPresent()) {

            fileName = existing.get().getFileName();
            thumbnailName = existing.get().getThumbnailFileName();

        } else {

            fileName = hash + extension;
            thumbnailName = "thumb_" + hash + ".jpg";

            Path shared = sharedDir(branchId);

            Path saved = shared.resolve(fileName);

            if (!Files.exists(saved)) {

                try (InputStream in = file.getInputStream()) {

                    saved = fileStorageService.saveFile(shared, fileName, in);
                    transactionalFileManager.track(saved);

                    Path thumb = createThumbnail(shared, fileName, saved);
                    transactionalFileManager.track(thumb);
                }
            }
        }

        boolean hasPrimary =
                imageRepo.findPrimaryImageFilename(
                                tenantId(),
                                branchId,
                                variant.getId()
                        )
                        .isPresent();

        ProductVariantImage savedImage =
                imageRepo.save(
                        ProductVariantImage.builder()
                                .primaryImage(!hasPrimary)
                        .variant(variant)
                        .fileName(fileName)
                        .thumbnailFileName(thumbnailName)
                        .filePath("/api/product-variants/" + variant.getId() + "/images/" + fileName)
                        .contentHash(hash)
                        .tenantId(tenantId())
                        .branchId(branchId)
                        .deleted(false)
                        .uploadedAt(LocalDateTime.now())
                        .build()
        );

        audit(savedImage, "UPLOAD", null
        );
    }

    @Transactional
    public void deleteVariantImage(
            UUID branchId,
            UUID variantId,
            String fileName,
            String reason
    ) {

        getVariant(branchId, variantId);

        ProductVariantImage image =
                imageRepo.findByTenantIdAndBranchIdAndVariant_IdAndFileNameAndDeletedFalse(
                                tenantId(),
                                branchId,
                                variantId,
                                fileName
                        )
                        .orElseThrow(() ->
                                new EntityNotFoundException(
                                        "Image not found"
                                )
                        );

        boolean wasPrimary =
                Boolean.TRUE.equals(
                        image.getPrimaryImage()
                );

        image.setDeleted(true);
        image.setPrimaryImage(false);

        imageRepo.save(image);

        if (wasPrimary) {
            promoteReplacementPrimary(
                    branchId,
                    variantId,
                    image.getId()
            );
        }

        audit(
                image,
                "SOFT_DELETE",
                reason
        );
    }

    @Transactional
    public void restoreVariantImage(
            UUID branchId,
            UUID variantId,
            String fileName,
            String reason
    ) {
        ProductVariant variant =
                getVariant(
                        branchId,
                        variantId
                );

        if (Boolean.TRUE.equals(variant.isDeleted())) {
            throw new IllegalStateException(
                    "Cannot restore image of a deleted variant"
            );
        }

        ProductVariantImage image =
                imageRepo.findByTenantIdAndBranchIdAndVariant_IdAndFileName(
                                tenantId(),
                                branchId,
                                variantId,
                                fileName
                        )
                        .orElseThrow();

        image.setDeleted(false);

        imageRepo.save(image);

        audit(
                image,
                "RESTORE",
                reason
        );
    }

    @Transactional
    public void hardDeleteVariantImage(
            UUID branchId,
            UUID variantId,
            String fileName,
            String reason
    ) {

        ProductVariantImage image =
                imageRepo
                        .findByTenantIdAndBranchIdAndVariant_IdAndFileName(
                                tenantId(),
                                branchId,
                                variantId,
                                fileName
                        )
                        .orElseThrow(() ->
                                new EntityNotFoundException(
                                        "Image not found"
                                )
                        );

        boolean wasPrimary =
                Boolean.TRUE.equals(
                        image.getPrimaryImage()
                );

        String hash =
                image.getContentHash();

        UUID imageId =
                image.getId();

        imageRepo.delete(image);

        if (wasPrimary) {
            promoteReplacementPrimary(
                    branchId,
                    variantId,
                    imageId
            );
        }

        long remaining =
                imageRepo
                        .countByTenantIdAndBranchIdAndContentHashAndDeletedFalse(
                                tenantId(),
                                branchId,
                                hash
                        );

        if (remaining == 0) {
            deletePhysicalFiles(
                    branchId,
                    image
            );
        }

        audit(
                image,
                "HARD_DELETE",
                reason
        );
    }

    private void promoteReplacementPrimary(
            UUID branchId,
            UUID variantId,
            UUID deletedImageId
    ) {

        List<ProductVariantImage> candidates =
                imageRepo.findReplacementCandidates(
                        tenantId(),
                        branchId,
                        variantId,
                        deletedImageId
                );

        if (candidates.isEmpty()) {
            return;
        }

        ProductVariantImage replacement =
                candidates.get(0);

        replacement.setPrimaryImage(true);

        imageRepo.save(
                replacement
        );
    }

    @Transactional
    public void hardDeleteAllImagesForVariant(
            UUID branchId,
            UUID variantId,
            String reason
    ) {

        List<ProductVariantImage> images =
                imageRepo.findByTenantIdAndBranchIdAndVariant_Id(
                        tenantId(),
                        branchId,
                        variantId
                );

        for (ProductVariantImage image : images) {

            String hash = image.getContentHash();

            imageRepo.delete(image);

            long remaining =
                    imageRepo.countByTenantIdAndBranchIdAndContentHashAndDeletedFalse(
                            tenantId(),
                            branchId,
                            hash
                    );

            if (remaining == 0) {

                deletePhysicalFiles(
                        branchId,
                        image
                );

            }

            audit(
                    image,
                    "HARD_DELETE",
                    reason
            );
        }
    }

    /* ============================================================
       READ
       ============================================================ */

    @Transactional(readOnly = true)
    public List<VariantImageDTO> getAllImages(
            UUID branchId,
            UUID variantId
    ) {

        getVariant(
                branchId,
                variantId
        );

        return imageRepo.findByTenantIdAndBranchIdAndVariant_Id(
                        tenantId(),
                        branchId,
                        variantId
                )
                .stream()
                .map(img ->
                        new VariantImageDTO(
                                img.getFileName(),
                                img.getFilePath(),
                                "/api/product-variants/"
                                        + variantId
                                        + "/thumbnails/"
                                        + img.getThumbnailFileName(),
                                Boolean.TRUE.equals(
                                        img.getPrimaryImage()
                                ),
                                img.isDeleted()
                        )
                )
                .toList();
    }

    public ResponseEntity<Resource> getThumbnail(
            UUID branchId,
            UUID variantId,
            String fileName
    ) {

        ProductVariantImage image =
                imageRepo.findByTenantIdAndBranchIdAndVariant_IdAndThumbnailFileNameAndDeletedFalse(
                                tenantId(),
                                branchId,
                                variantId,
                                fileName
                        )
                        .orElseThrow(
                                () ->
                                        new ResponseStatusException(
                                                HttpStatus.NOT_FOUND
                                        )
                        );

        Path file =
                sharedDir(branchId)
                        .resolve(
                                image.getThumbnailFileName()
                        );

        return ResponseEntity.ok()
                .contentType(
                        MediaTypeFactory
                                .getMediaType(file.toString())
                                .orElse(
                                        MediaType.APPLICATION_OCTET_STREAM
                                )
                )
                .body(
                        new FileSystemResource(file)
                );
    }

    @Transactional(readOnly = true)
    public List<String> getImageUrls(UUID branchId, UUID variantId) {

        getVariant(branchId, variantId);

        return imageRepo.findByTenantIdAndBranchIdAndVariant_IdAndDeletedFalse(
                        tenantId(),
                        branchId,
                        variantId
                )
                .stream()
                .map(ProductVariantImage::getFilePath)
                .toList();
    }

    public ResponseEntity<Resource> getProductVariantImage(
            UUID branchId,
            UUID variantId,
            String fileName
    ) {

        getVariant(branchId, variantId);

        ProductVariantImage img = imageRepo
                .findByTenantIdAndBranchIdAndVariant_IdAndFileName(
                        tenantId(),
                        branchId,
                        variantId,
                        fileName
                )
                .orElseThrow(() ->
                        new ResponseStatusException(
                                HttpStatus.NOT_FOUND
                        ));

        Path path =
                sharedDir(branchId)
                        .resolve(img.getFileName());

        if (!Files.exists(path)) {
            throw new ResponseStatusException(
                    HttpStatus.NOT_FOUND
            );
        }

        return ResponseEntity.ok()
                .contentType(
                        MediaTypeFactory
                                .getMediaType(
                                        path.toString()
                                )
                                .orElse(
                                        MediaType.APPLICATION_OCTET_STREAM
                                )
                )
                .body(
                        new FileSystemResource(path)
                );
    }

    @Transactional(readOnly = true)
    public ResponseEntity<Resource> getSharedThumbnail(
            UUID branchId,
            String fileName
    ) {

        Path file =
                sharedDir(branchId)
                        .resolve(fileName);

        if (!Files.exists(file)) {
            return ResponseEntity.notFound().build();
        }

        try {

            String etag =
                    "\"" +
                            Files.getLastModifiedTime(file)
                                    .toMillis()
                            + "\"";

            Resource resource =
                    new FileSystemResource(
                            file
                    );

            return ResponseEntity.ok()
                    .eTag(etag)
                    .header(
                            HttpHeaders.CACHE_CONTROL,
                            "public,max-age=86400"
                    )
                    .contentType(
                            MediaType.IMAGE_JPEG
                    )
                    .body(resource);

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public ResponseEntity<Resource> downloadZipResponse(UUID branchId, UUID variantId) throws IOException {

        getVariant(branchId, variantId);

        List<ProductVariantImage> images =
                imageRepo.findByTenantIdAndBranchIdAndVariant_Id(
                        tenantId(),
                        branchId,
                        variantId
                );

        File zip = File.createTempFile("variant-images-", ".zip");

        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(zip))) {

            for (ProductVariantImage img : images) {

                Path p = sharedDir(branchId).resolve(img.getFileName());

                if (!Files.exists(p)) continue;

                zos.putNextEntry(new ZipEntry(img.getFileName()));
                Files.copy(p, zos);
                zos.closeEntry();
            }
        }

        return ResponseEntity.ok()
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=images.zip")
                .body(new FileSystemResource(zip));
    }

    /* ============================================================
       UTIL
       ============================================================ */

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
            throw new RuntimeException(e);
        }
    }

    private void validateFile(MultipartFile file) {
        if (file.getSize() > 10 * 1024 * 1024) {
            throw new IllegalArgumentException("Max 10MB");
        }
        if (file.getContentType() == null ||
                !file.getContentType().startsWith("image/")) {
            throw new IllegalArgumentException("Only image files allowed");
        }
    }

    private String getExtension(String name) {
        if (name == null || !name.contains(".")) return ".bin";
        return name.substring(name.lastIndexOf("."));
    }

    private Path createThumbnail(Path dir, String fileName, Path source) throws IOException {

        String base = fileName.replaceAll("\\.[^.]+$", "");
        String thumb = "thumb_" + base + ".jpg";

        Path path = dir.resolve(thumb);

        Thumbnails.of(source.toFile())
                .size(300, 300)
                .outputFormat("jpg")
                .toFile(path.toFile());

        fileStorageService.secure(path);

        return path;
    }

    private void deletePhysicalFiles(
            UUID branchId,
            ProductVariantImage image
    ) {
        try {

            Path shared = sharedDir(branchId);

            Path original =
                    shared.resolve(
                            image.getFileName()
                    );

            Path thumbnail =
                    shared.resolve(
                            image.getThumbnailFileName()
                    );

            fileStorageService.deleteFile(original);
            fileStorageService.deleteFile(thumbnail);

        } catch (Exception e) {

            throw new RuntimeException(
                    "Failed deleting image files",
                    e
            );

        }
    }

    @Transactional(readOnly = true)
    public List<VariantImageAuditDTO> getImageAuditHistory(
            UUID branchId,
            UUID variantId
    ) {
        getVariant(
                branchId,
                variantId
        );

        return auditRepo
                .findByTenantIdAndBranchIdAndProductVariantIdIn(
                        tenantId(),
                        branchId,
                        List.of(variantId)
                )
                .stream()
                .sorted(
                        Comparator.comparing(
                                ProductVariantImageAudit::getTimestamp
                        ).reversed()
                )
                .map(a ->
                        VariantImageAuditDTO.builder()
                                .id(a.getId())
                                .productVariantId(a.getProductVariantId())
                                .productName(a.getProductName())
                                .classification(a.getClassification())
                                .fileName(a.getFileName())
                                .filePath(a.getFilePath())
                                .action(a.getAction())
                                .reason(a.getReason())
                                .timestamp(a.getTimestamp())
                                .performedBy(a.getPerformedBy())
                                .build()
                )
                .toList();
    }

    private void audit(
            ProductVariantImage image,
            String action,
            String reason
    ) {

        ProductVariant variant =
                image.getVariant();

        auditRepo.save(
                ProductVariantImageAudit
                        .builder()
                        .tenantId(tenantId())
                        .branchId(
                                image.getBranchId()
                        )
                        .productVariantId(
                                variant.getId()
                        )
                        .productName(
                                variant
                                        .getProduct()
                                        .getName()
                        )
                        .classification(
                                variant.getClassification()
                        )
                        .fileName(
                                image.getFileName()
                        )
                        .filePath(
                                image.getFilePath()
                        )
                        .action(action)
                        .reason(reason)
                        .timestamp(
                                LocalDateTime.now()
                        )
                        .performedBy(
                                SecurityUtils.currentUsername()
                        )
                        .build()
        );
    }
}