package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.service;

import com.IntegrityTechnologies.business_manager.config.FileStorageProperties;
import com.IntegrityTechnologies.business_manager.config.FileStorageService;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariantImage;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantImageRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

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

    private Path root() {
        return Paths.get(props.getProductUploadDir()).toAbsolutePath().normalize();
    }

    @Transactional
    public void uploadVariantImages(UUID variantId, List<MultipartFile> files) throws IOException {

        ProductVariant variant = variantRepo.findById(variantId)
                .orElseThrow(() -> new EntityNotFoundException("Variant not found"));

        Path dir = root()
                .resolve(variant.getProduct().getId().toString())
                .resolve("variants")
                .resolve(variantId.toString());

        fileStorageService.initDirectory(dir);

        for (MultipartFile file : files) {
            if (file.isEmpty()) continue;

            String name = System.currentTimeMillis() + "_" +
                    UUID.randomUUID() + "_" +
                    file.getOriginalFilename().replaceAll("[^a-zA-Z0-9._-]", "_");

            try (InputStream in = file.getInputStream()) {
                Path saved = fileStorageService.saveFile(dir, name, in);
                fileStorageService.hidePath(saved);

                imageRepo.save(
                        ProductVariantImage.builder()
                                .variant(variant)
                                .fileName(name)
                                .filePath("/api/product-variants/images/" + variantId + "/" + name)
                                .build()
                );
            }
        }
    }

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

        Path variantDir = Paths.get(props.getProductUploadDir())
                .resolve(variant.getProduct().getId().toString())
                .resolve("variants")
                .resolve(variantId.toString())
                .toAbsolutePath()
                .normalize();

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
}