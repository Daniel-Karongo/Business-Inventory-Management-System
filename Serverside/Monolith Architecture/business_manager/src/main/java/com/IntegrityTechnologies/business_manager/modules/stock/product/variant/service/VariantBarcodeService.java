package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.service;

import com.IntegrityTechnologies.business_manager.config.FileStorageProperties;
import com.IntegrityTechnologies.business_manager.config.FileStorageService;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.dto.ProductVariantDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.mapper.ProductVariantMapper;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
import com.google.zxing.BarcodeFormat;
import com.google.zxing.client.j2se.MatrixToImageWriter;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.oned.Code128Writer;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Slf4j
public class VariantBarcodeService {

    private final ProductVariantRepository variantRepository;
    private final ProductVariantMapper mapper;
    private final FileStorageService fileStorageService;

    /* =============================
       PUBLIC API
       ============================= */

    public ProductVariantDTO getVariantByBarcode(String barcode) {
        if (!isValidBarcode(barcode))
            throw new IllegalArgumentException("Invalid barcode format");

        ProductVariant variant = variantRepository
                .findByBarcode(barcode)
                .orElseThrow(() ->
                        new EntityNotFoundException("No active variant for barcode: " + barcode)
                );

        return mapper.toDTO(variant);
    }

    @Transactional
    public ProductVariantDTO generateBarcodeIfMissing(UUID variantId) {

        ProductVariant variant = variantRepository.findById(variantId)
                .orElseThrow(() -> new EntityNotFoundException("Variant not found"));

        if (variant.getBarcode() == null || variant.getBarcode().isBlank()) {

            String barcode;
            do {
                barcode = autoGenerateBarcode();
            } while (variantRepository.findByBarcode(barcode).isPresent());

            variant.setBarcode(barcode);

            Path imagePath = generateBarcodeImage(barcode, variant);
            variant.setBarcodeImagePath(
                    "/api/product-variants/" +
                            variant.getId() +
                            "/barcode/" +
                            barcode +
                            ".png"
            );

            variantRepository.save(variant);
        }

        return mapper.toDTO(variant);
    }

    /* =============================
       VALIDATION
       ============================= */

    public boolean isValidBarcode(String barcode) {
        return barcode != null && barcode.matches("^[A-Z0-9]{8,20}$");
    }

    public String autoGenerateBarcode() {
        return UUID.randomUUID()
                .toString()
                .replace("-", "")
                .substring(0, 12)
                .toUpperCase();
    }

    /* =============================
       IMAGE GENERATION
       ============================= */

    public Path generateBarcodeImage(String barcode, ProductVariant variant) {

        try {
            Path dir = initAndHideVariantBarcodeDir(variant);

            Files.createDirectories(dir);
            fileStorageService.hidePathIfSupported(dir);

            Path output = dir.resolve(barcode + ".png");

            Code128Writer writer = new Code128Writer();
            BitMatrix matrix = writer.encode(barcode, BarcodeFormat.CODE_128, 400, 120);
            MatrixToImageWriter.writeToPath(matrix, "PNG", output);

            fileStorageService.hidePathIfSupported(output);
            log.info("Barcode image created at {}", output);

            return output;

        } catch (Exception e) {
            throw new RuntimeException("Failed to generate barcode image", e);
        }
    }

    private Path initAndHideVariantBarcodeDir(ProductVariant variant) throws IOException {

        Path barcodeDir =
                fileStorageService.productRoot()
                        .resolve(variant.getProduct().getId().toString())
                        .resolve("variants")
                        .resolve(variant.getId().toString())
                        .resolve("barcode")
                        .normalize();

        fileStorageService.initDirectory(barcodeDir);

        // One call is enough — UploadsInitializer already hid parents
        fileStorageService.hidePathIfSupported(barcodeDir);

        return barcodeDir;
    }
}