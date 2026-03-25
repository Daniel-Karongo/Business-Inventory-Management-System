package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.service;

import com.IntegrityTechnologies.business_manager.config.files.FileStorageService;
import com.IntegrityTechnologies.business_manager.config.files.TransactionalFileManager;
import com.IntegrityTechnologies.business_manager.config.kafka.OutboxEventWriter;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariantImage;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.repository.ProductVariantImageRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.repository.ProductVariantRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.events.VariantImageUploadRequestedEvent;
import com.IntegrityTechnologies.business_manager.security.util.BranchContext;
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

import java.io.*;
import java.nio.file.*;
import java.security.MessageDigest;
import java.time.LocalDateTime;
import java.util.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

@Service
@RequiredArgsConstructor
public class ProductVariantImageService {

    private final ProductVariantRepository variantRepo;
    private final ProductVariantImageRepository imageRepo;
    private final FileStorageService fileStorageService;
    private final TransactionalFileManager transactionalFileManager;
    private final OutboxEventWriter outboxEventWriter;

    private UUID tenantId() { return TenantContext.getTenantId(); }
    private UUID branchId() { return BranchContext.get(); }

    private ProductVariant getVariant(UUID variantId) {
        return variantRepo.findByIdSafe(
                variantId,
                false,
                tenantId(),
                branchId()
        ).orElseThrow(() -> new EntityNotFoundException("Variant not found"));
    }

    private Path sharedDir() {
        return fileStorageService.initDirectory(
                fileStorageService.productSharedRoot().resolve("_variant")
        );
    }

    /* ============================================================
       UPLOAD
       ============================================================ */

    @Transactional
    public void uploadVariantImages(UUID variantId, List<MultipartFile> files) throws IOException {

        ProductVariant variant = getVariant(variantId);

        if (files == null || files.isEmpty()) return;

        for (MultipartFile file : files) {

            validateFile(file);

            Path tempDir = fileStorageService.initDirectory(
                    fileStorageService.productSharedRoot().resolve("_tmp")
            );

            String tempName = UUID.randomUUID() + "_" + file.getOriginalFilename();

            Path tempFile = tempDir.resolve(tempName);

            try (InputStream in = file.getInputStream()) {
                Files.copy(in, tempFile);
            }

            outboxEventWriter.write(
                    "VARIANT_IMAGE_UPLOAD_REQUESTED",
                    branchId(),
                    VariantImageUploadRequestedEvent.builder()
                            .variantId(variantId)
                            .tenantId(tenantId())
                            .branchId(branchId())
                            .fileName(file.getOriginalFilename())
                            .tempFilePath(tempFile.toString())
                            .build()
            );
        }
    }

    @Transactional
    public void saveVariantImage(ProductVariant variant, MultipartFile file) throws IOException {

        if (file == null || file.isEmpty()) return;

        validateFile(file);

        String hash = computeHash(file);

        Optional<ProductVariantImage> existing =
                imageRepo.findFirstByTenantIdAndBranchIdAndContentHash(
                        tenantId(),
                        branchId(),
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

            Path shared = sharedDir();

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

        imageRepo.save(
                ProductVariantImage.builder()
                        .variant(variant)
                        .fileName(fileName)
                        .thumbnailFileName(thumbnailName)
                        .filePath("/api/product-variants/" + variant.getId() + "/images/" + fileName)
                        .contentHash(hash)
                        .tenantId(tenantId())
                        .branchId(branchId())
                        .deleted(false)
                        .uploadedAt(LocalDateTime.now())
                        .build()
        );
    }

    /* ============================================================
       READ
       ============================================================ */

    @Transactional(readOnly = true)
    public List<String> getImageUrls(UUID variantId) {

        getVariant(variantId);

        return imageRepo.findByTenantIdAndBranchIdAndVariant_IdAndDeletedFalse(
                        tenantId(),
                        branchId(),
                        variantId
                )
                .stream()
                .map(ProductVariantImage::getFilePath)
                .toList();
    }

    public ResponseEntity<Resource> getProductVariantImage(UUID variantId, String fileName) {

        getVariant(variantId);

        ProductVariantImage img = imageRepo
                .findByTenantIdAndBranchIdAndVariant_IdAndFileNameAndDeletedFalse(
                        tenantId(),
                        branchId(),
                        variantId,
                        fileName
                )
                .orElseThrow(() -> new ResponseStatusException(HttpStatus.NOT_FOUND));

        Path path = sharedDir().resolve(img.getFileName());

        if (!Files.exists(path)) {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND);
        }

        return ResponseEntity.ok(new FileSystemResource(path));
    }

    public ResponseEntity<Resource> downloadZipResponse(UUID variantId) throws IOException {

        getVariant(variantId);

        List<ProductVariantImage> images =
                imageRepo.findByTenantIdAndBranchIdAndVariant_IdAndDeletedFalse(
                        tenantId(),
                        branchId(),
                        variantId
                );

        File zip = File.createTempFile("variant-images-", ".zip");

        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(zip))) {

            for (ProductVariantImage img : images) {

                Path p = sharedDir().resolve(img.getFileName());

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
}