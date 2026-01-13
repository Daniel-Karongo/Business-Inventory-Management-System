package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.service;

import com.IntegrityTechnologies.business_manager.config.FileStorageProperties;
import com.IntegrityTechnologies.business_manager.config.FileStorageService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariantImage;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantImageRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.nio.file.*;
import java.time.LocalDateTime;
import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
public class ProductImageMigrationService {

    private final ProductRepository productRepository;
    private final ProductVariantRepository variantRepository;
    private final ProductVariantImageRepository variantImageRepository;
    private final FileStorageProperties props;
    private final FileStorageService fileStorageService;

    private Path productRoot() {
        return Paths.get(props.getProductUploadDir())
                .toAbsolutePath()
                .normalize();
    }

    @Transactional
    public void migrate() {

        List<Product> products = productRepository.findAll();

        for (Product product : products) {

            Path productDir = productRoot().resolve(product.getId().toString());
            if (!Files.exists(productDir) || !Files.isDirectory(productDir)) {
                continue;
            }

            List<ProductVariant> variants =
                    variantRepository.findByProduct_Id(product.getId());

            if (variants.isEmpty()) continue;

            try (DirectoryStream<Path> stream = Files.newDirectoryStream(productDir)) {

                for (Path productFile : stream) {

                    // Skip directories (variants/, barcode/, etc.)
                    if (Files.isDirectory(productFile)) continue;

                    String fileName = productFile.getFileName().toString();

                    for (ProductVariant variant : variants) {

                        boolean exists =
                                variantImageRepository
                                        .existsByVariant_IdAndFileName(
                                                variant.getId(), fileName);

                        if (exists) continue;

                        Path variantDir = productDir
                                .resolve("variants")
                                .resolve(variant.getId().toString())
                                .resolve("images");

                        fileStorageService.initDirectory(variantDir);
                        fileStorageService.hidePathIfSupported(variantDir);

                        Path target = variantDir.resolve(fileName);

                        Files.copy(
                                productFile,
                                target,
                                StandardCopyOption.REPLACE_EXISTING
                        );

                        fileStorageService.hidePathIfSupported(target);

                        String apiPath =
                                "/api/product-variants/" +
                                        variant.getId() +
                                        "/images/" +
                                        fileName;

                        ProductVariantImage img =
                                ProductVariantImage.builder()
                                        .variant(variant)
                                        .fileName(fileName)
                                        .filePath(apiPath)
                                        .uploadedAt(LocalDateTime.now())
                                        .deleted(false)
                                        .build();

                        variantImageRepository.save(img);
                    }
                }

            } catch (Exception e) {
                throw new RuntimeException(
                        "Failed migrating images for product " + product.getId(),
                        e
                );
            }
        }

        log.info("✅ Filesystem-based product → variant image migration completed");
    }
}