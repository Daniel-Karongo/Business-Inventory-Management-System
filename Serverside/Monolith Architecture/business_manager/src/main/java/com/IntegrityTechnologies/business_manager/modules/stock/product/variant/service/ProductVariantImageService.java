package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.service;

import com.IntegrityTechnologies.business_manager.config.FileStorageProperties;
import com.IntegrityTechnologies.business_manager.config.FileStorageService;
import com.IntegrityTechnologies.business_manager.config.TransactionalFileManager;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariantImage;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantImageRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
import lombok.RequiredArgsConstructor;
import net.coobird.thumbnailator.Thumbnails;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
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
import java.nio.file.Paths;
import java.util.List;
import java.util.UUID;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

@Service
@RequiredArgsConstructor
public class ProductVariantImageService {

    private final ProductVariantRepository variantRepo;
    private final ProductVariantImageRepository imageRepo;
    private final FileStorageService fileStorageService;
    private final FileStorageProperties props;
    private final TransactionalFileManager transactionalFileManager;

    /* ============================================================
       ROOT DIRECTORY
       ============================================================ */

    private Path root() {
        return Paths.get(props.getProductUploadDir())
                .toAbsolutePath()
                .normalize();
    }

    /* ============================================================
       PUBLIC API
       ============================================================ */

    @Transactional
    public void uploadVariantImages(UUID variantId, List<MultipartFile> files) throws IOException {

        ProductVariant variant = variantRepo.findById(variantId)
                .orElseThrow(() -> new EntityNotFoundException("Variant not found"));

        if (files == null || files.isEmpty()) return;

        for (MultipartFile file : files) {
            saveVariantImage(variant, file);
        }
    }

    @Transactional
    public void saveVariantImage(ProductVariant variant, MultipartFile file) throws IOException {

        if (file == null || file.isEmpty()) return;

        Path variantDir = root()
                .resolve(variant.getProduct().getId().toString())
                .resolve("variants")
                .resolve(variant.getId().toString());

        fileStorageService.initDirectory(variantDir);
        fileStorageService.hidePathIfSupported(variantDir);

        String sanitized = sanitize(file.getOriginalFilename());

        String fileName =
                System.currentTimeMillis() + "_" +
                        UUID.randomUUID() + "_" +
                        sanitized;

        Path saved;

        /* =========================
           SAVE ORIGINAL
           ========================= */

        try (InputStream in = file.getInputStream()) {

            saved = fileStorageService.saveFile(variantDir, fileName, in);

            transactionalFileManager.track(saved);
            fileStorageService.hidePath(saved);
        }

        /* =========================
           GENERATE THUMBNAIL
           ========================= */

        String thumbnailName = "thumb_" + fileName;
        Path thumbnailPath = variantDir.resolve(thumbnailName);

        Thumbnails.of(saved.toFile())
                .size(300, 300)
                .outputFormat("jpg")
                .toFile(thumbnailPath.toFile());

        transactionalFileManager.track(thumbnailPath);
        fileStorageService.hidePath(thumbnailPath);

        /* =========================
           SAVE METADATA
           ========================= */

        imageRepo.save(
                ProductVariantImage.builder()
                        .variant(variant)
                        .fileName(fileName)
                        .filePath("/api/product-variants/images/"
                                + variant.getId() + "/" + fileName)
                        .thumbnailFileName(thumbnailName)
                        .deleted(false)
                        .build()
        );
    }

    /* ============================================================
       READ
       ============================================================ */

    @Transactional(readOnly = true)
    public List<String> getImageUrls(UUID variantId) {

        return imageRepo.findByVariant_IdAndDeletedFalse(variantId)
                .stream()
                .map(ProductVariantImage::getFilePath)
                .toList();
    }

    @Transactional(readOnly = true)
    public File zipVariantImages(UUID variantId) throws IOException {

        ProductVariant variant = variantRepo.findById(variantId)
                .orElseThrow(() -> new EntityNotFoundException("Variant not found"));

        Path variantDir = root()
                .resolve(variant.getProduct().getId().toString())
                .resolve("variants")
                .resolve(variantId.toString());

        List<ProductVariantImage> images =
                imageRepo.findByVariant_IdAndDeletedFalse(variantId);

        File zip = File.createTempFile(
                "variant-" + variantId + "-images",
                ".zip"
        );

        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(zip))) {

            for (ProductVariantImage img : images) {

                Path physical = variantDir.resolve(img.getFileName());

                if (!Files.exists(physical)) continue;

                zos.putNextEntry(new ZipEntry(img.getFileName()));
                Files.copy(physical, zos);
                zos.closeEntry();
            }
        }

        return zip;
    }

    public ResponseEntity<Resource> getProductVariantImage(UUID variantId, String fileName) {

        ProductVariantImage img = imageRepo
                .findByVariant_IdAndDeletedFalse(variantId)
                .stream()
                .filter(i -> i.getFileName().equals(fileName))
                .findFirst()
                .orElseThrow(() ->
                        new ResponseStatusException(
                                HttpStatus.NOT_FOUND,
                                "Variant image metadata not found"
                        )
                );

        ProductVariant variant = img.getVariant();

        Path imagePath = root()
                .resolve(variant.getProduct().getId().toString())
                .resolve("variants")
                .resolve(variantId.toString())
                .resolve(img.getFileName())
                .normalize();

        if (!Files.exists(imagePath)) {
            throw new ResponseStatusException(
                    HttpStatus.NOT_FOUND,
                    "Variant image not found on disk"
            );
        }

        Resource resource = new FileSystemResource(imagePath.toFile());

        MediaType mediaType = MediaType.APPLICATION_OCTET_STREAM;

        try {
            String detected = Files.probeContentType(imagePath);
            if (detected != null) {
                mediaType = MediaType.parseMediaType(detected);
            }
        } catch (IOException ignored) {}

        return ResponseEntity.ok()
                .contentType(mediaType)
                .body(resource);
    }

    /* ============================================================
       UTILITIES
       ============================================================ */

    private String sanitize(String original) {

        if (original == null || original.isBlank()) {
            return "file";
        }

        return original.replaceAll("[^a-zA-Z0-9._-]", "_");
    }
}