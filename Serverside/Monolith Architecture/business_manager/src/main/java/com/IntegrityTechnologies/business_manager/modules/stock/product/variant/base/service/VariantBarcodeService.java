package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.service;

import com.IntegrityTechnologies.business_manager.config.files.FileStorageService;
import com.IntegrityTechnologies.business_manager.config.kafka.OutboxEventWriter;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.mapper.ProductVariantMapper;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.ProductVariantDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.events.VariantBarcodeRequestedEvent;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.repository.ProductVariantRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchContext;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import com.google.zxing.BarcodeFormat;
import com.google.zxing.client.j2se.MatrixToImageWriter;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.oned.Code128Writer;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.io.Resource;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Slf4j
public class VariantBarcodeService {

    private final ProductVariantRepository variantRepository;
    private final ProductVariantMapper mapper;
    private final FileStorageService fileStorageService;
    private final OutboxEventWriter outboxEventWriter;

    private UUID tenantId() { return TenantContext.getTenantId(); }
    private UUID branchId() { return BranchContext.get(); }

    /* =============================
       PUBLIC API
       ============================= */

    public ProductVariantDTO getVariantByBarcode(String barcode) {

        if (!isValidBarcode(barcode))
            throw new IllegalArgumentException("Invalid barcode format");

        ProductVariant variant = variantRepository
                .findForScan(tenantId(), branchId(), barcode)
                .orElseThrow(() ->
                        new EntityNotFoundException("No active variant for barcode: " + barcode)
                );

        return mapper.toDTO(variant);
    }

    @Transactional
    public ProductVariantDTO generateBarcodeIfMissing(UUID variantId) {

        ProductVariant variant = variantRepository.findByIdSafe(
                variantId,
                false,
                tenantId(),
                branchId()
        ).orElseThrow(() -> new EntityNotFoundException("Variant not found"));

        if (variant.getBarcode() == null || variant.getBarcode().isBlank()) {

            String barcode = autoGenerateBarcode();
            variant.setBarcode(barcode);

            try {

                variantRepository.saveAndFlush(variant);

            } catch (org.springframework.dao.DataIntegrityViolationException ex) {

                // retry once
                barcode = autoGenerateBarcode();
                variant.setBarcode(barcode);
                variantRepository.saveAndFlush(variant);
            }

            outboxEventWriter.write(
                    "VARIANT_BARCODE_REQUESTED",
                    branchId(),
                    VariantBarcodeRequestedEvent.builder()
                            .variantId(variant.getId())
                            .tenantId(tenantId())
                            .branchId(branchId())
                            .build()
            );

            variant.setBarcodeImagePath(
                    "/api/product-variants/" +
                            variant.getId() +
                            "/barcode/image"
            );

            variantRepository.save(variant);
        }

        return mapper.toDTO(variant);
    }

    /* =============================
       SAFE FILE SERVING
       ============================= */

    public ResponseEntity<Resource> getBarcodeImage(UUID variantId) {

        ProductVariant variant = variantRepository.findByIdSafe(
                variantId,
                false,
                tenantId(),
                branchId()
        ).orElseThrow(() -> new EntityNotFoundException("Variant not found"));

        if (variant.getBarcode() == null) {
            throw new EntityNotFoundException("Barcode not generated");
        }

        try {

            Path path = resolveBarcodePath(variant);

            if (!Files.exists(path)) {
                throw new EntityNotFoundException("Barcode image not found");
            }

            Resource resource = fileStorageService.asResource(path);

            return ResponseEntity.ok(resource);

        } catch (Exception e) {
            throw new RuntimeException("Failed to load barcode image", e);
        }
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

            Path barcodeDir = resolveBarcodeDir(variant);

            Path output = barcodeDir.resolve(barcode + ".png");

            // ✅ IDPOTENCY CHECK
            if (Files.exists(output)) {
                return output;
            }

            Code128Writer writer = new Code128Writer();
            BitMatrix matrix = writer.encode(barcode, BarcodeFormat.CODE_128, 400, 120);
            MatrixToImageWriter.writeToPath(matrix, "PNG", output);

            fileStorageService.secure(output);

            log.info("Barcode image created at {}", output);

            return output;

        } catch (Exception e) {
            throw new RuntimeException("Failed to generate barcode image", e);
        }
    }

    public void ensureBarcodeExists(ProductVariant variant) {

        if (variant.getBarcode() == null || variant.getBarcode().isBlank()) {

            String barcode = autoGenerateBarcode();
            variant.setBarcode(barcode);

            try {

                variantRepository.saveAndFlush(variant);

            } catch (org.springframework.dao.DataIntegrityViolationException ex) {

                barcode = autoGenerateBarcode();
                variant.setBarcode(barcode);
                variantRepository.saveAndFlush(variant);
            }
        }
    }
    /* =============================
       INTERNAL PATH HELPERS (SAFE)
       ============================= */

    private Path resolveBarcodeDir(ProductVariant variant) {

        return fileStorageService.initDirectory(
                fileStorageService.productRoot()
                        .resolve(variant.getProduct().getId().toString())
                        .resolve("variants")
                        .resolve(variant.getId().toString())
                        .resolve("barcode")
        );
    }

    private Path resolveBarcodePath(ProductVariant variant) {

        return fileStorageService.productRoot()
                .resolve(variant.getProduct().getId().toString())
                .resolve("variants")
                .resolve(variant.getId().toString())
                .resolve("barcode")
                .resolve(variant.getBarcode() + ".png");
    }
}