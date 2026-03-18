package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.service;

import com.IntegrityTechnologies.business_manager.config.files.InMemoryMultipartFile;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.events.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchContext;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Slf4j
public class VariantAsyncProcessor {

    private final ProductVariantRepository variantRepo;
    private final VariantBarcodeService barcodeService;
    private final ProductVariantImageService imageService;
    private final VariantBarcodePdfService pdfService;

    public void processBarcode(VariantBarcodeRequestedEvent event) {

        try {

            TenantContext.setTenantId(event.getTenantId());
            BranchContext.set(event.getBranchId());

            ProductVariant variant =
                    variantRepo.findByIdSafe(
                            event.getVariantId(),
                            false,
                            event.getTenantId(),
                            event.getBranchId()
                    ).orElseThrow();

            barcodeService.generateBarcodeImage(
                    variant.getBarcode(),
                    variant
            );

            log.info("Async barcode generated {}", variant.getId());

        } catch (Exception e) {
            log.error(
                    "Async barcode generation failed. tenant={}, branch={}, variant={}",
                    event.getTenantId(),
                    event.getBranchId(),
                    event.getVariantId(),
                    e
            );

            throw new RuntimeException(e); // ✅ let outbox retry
        } finally {
            TenantContext.clear();
            BranchContext.clear();
        }
    }

    public void processImage(VariantImageUploadRequestedEvent event) {

        try {

            TenantContext.setTenantId(event.getTenantId());
            BranchContext.set(event.getBranchId());

            ProductVariant variant =
                    variantRepo.findByIdSafe(
                            event.getVariantId(),
                            false,
                            event.getTenantId(),
                            event.getBranchId()
                    ).orElseThrow();

            Path tempFile = Path.of(event.getTempFilePath());

            try {

                MultipartFile file =
                        new InMemoryMultipartFile(
                                event.getFileName(),
                                Files.readAllBytes(tempFile)
                        );

                imageService.saveVariantImage(variant, file);

            } finally {

                try {
                    Files.deleteIfExists(tempFile);
                } catch (Exception ignored) {}
            }

            log.info("Async image processed {}", variant.getId());

        } catch (Exception e) {

            log.error(
                    "Failed async image processing. tenant={}, branch={}, variant={}",
                    event.getTenantId(),
                    event.getBranchId(),
                    event.getVariantId(),
                    e
            );
            throw new RuntimeException(e); // ✅ let outbox retry

        } finally {
            TenantContext.clear();
            BranchContext.clear();
        }
    }

    public void processPdf(VariantBarcodePdfRequestedEvent event) {

        try {

            TenantContext.setTenantId(event.getTenantId());
            BranchContext.set(event.getBranchId());

            if (event.getVariantIds() != null) {

                pdfService.generateSheetToFile(
                        event.getVariantIds(),
                        event.getOutputPath()
                );

            } else {

                List<UUID> ids = variantRepo.findByProduct_IdSafe(
                        event.getProductId(),
                        false,
                        event.getTenantId(),
                        event.getBranchId()
                ).stream().map(ProductVariant::getId).toList();

                pdfService.generateSheetToFile(ids, event.getOutputPath());
            }

            log.info("PDF generated at {}", event.getOutputPath());

        } catch (Exception e) {

            log.error(
                    "Failed async PDF generation. tenant={}, branch={}, output={}, productId={}",
                    event.getTenantId(),
                    event.getBranchId(),
                    event.getOutputPath(),
                    event.getProductId(),
                    e
            );
            throw new RuntimeException(e);

        } finally {
            TenantContext.clear();
            BranchContext.clear();
        }
    }
}