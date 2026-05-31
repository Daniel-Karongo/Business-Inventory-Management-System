package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.service;

import com.IntegrityTechnologies.business_manager.config.files.InMemoryMultipartFile;
import com.IntegrityTechnologies.business_manager.config.kafka.ProcessedKafkaEventRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.events.VariantBarcodePdfRequestedEvent;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.events.VariantBarcodeRequestedEvent;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.events.VariantImageUploadRequestedEvent;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.repository.ProductVariantRepository;
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
    private final ProcessedKafkaEventRepository processedRepo;

    public void processBarcode(VariantBarcodeRequestedEvent event) {

        try {

            TenantContext.setTenantId(event.getTenantId());

            int claimed =
                    processedRepo.tryClaim(
                            UUID.randomUUID(),
                            event.getTenantId(),
                            event.getVariantId(),
                            "VARIANT_BARCODE"
                    );

            if (claimed == 0) {

                log.debug(
                        "Duplicate barcode replay suppressed for variant {}",
                        event.getVariantId()
                );

                return;
            }

            ProductVariant variant =
                    variantRepo.findByIdSafe(
                            event.getVariantId(),
                            false,
                            event.getTenantId(),
                            event.getBranchId()
                    ).orElseThrow();

            barcodeService.generateBarcodeImage(
                    event.getBranchId(),
                    variant.getBarcode(),
                    variant
            );

            log.info(
                    "Async barcode generated {}",
                    variant.getId()
            );

        } catch (Exception e) {

            log.error(
                    "Async barcode generation failed. tenant={}, branch={}, variant={}",
                    event.getTenantId(),
                    event.getBranchId(),
                    event.getVariantId(),
                    e
            );

            throw new RuntimeException(e);

        } finally {

            TenantContext.clear();
        }
    }

    public void processImage(VariantImageUploadRequestedEvent event) {

        try {

            TenantContext.setTenantId(event.getTenantId());

            int claimed =
                    processedRepo.tryClaim(
                            UUID.randomUUID(),
                            event.getTenantId(),
                            event.getUploadId(),
                            "VARIANT_IMAGE"
                    );

            if (claimed == 0) {

                log.debug(
                        "Duplicate image replay suppressed for variant {}",
                        event.getVariantId()
                );

                return;
            }

            ProductVariant variant =
                    variantRepo.findByIdSafe(
                            event.getVariantId(),
                            false,
                            event.getTenantId(),
                            event.getBranchId()
                    ).orElseThrow();

            Path tempFile = Path.of(event.getTempFilePath());

            boolean success = false;

            try {

                MultipartFile file =
                        new InMemoryMultipartFile(
                                event.getFileName(),
                                event.getContentType(),
                                Files.readAllBytes(tempFile)
                        );

                imageService.saveVariantImage(
                        event.getBranchId(),
                        variant,
                        file
                );

                success = true;

            } finally {

                if (success) {
                    try {
                        Files.deleteIfExists(tempFile);
                    } catch (Exception ignored) {
                    }
                }

            }

            log.info(
                    "Async image processed {}",
                    variant.getId()
            );

        } catch (Exception e) {

            log.error(
                    "Failed async image processing. tenant={}, branch={}, variant={}",
                    event.getTenantId(),
                    event.getBranchId(),
                    event.getVariantId(),
                    e
            );

            throw new RuntimeException(e);

        } finally {

            TenantContext.clear();
        }
    }

    public void processPdf(VariantBarcodePdfRequestedEvent event) {

        try {

            TenantContext.setTenantId(event.getTenantId());

            UUID replayKey =
                    event.getProductId() != null
                            ? event.getProductId()
                            : UUID.nameUUIDFromBytes(
                            event.getOutputPath().getBytes()
                    );

            int claimed =
                    processedRepo.tryClaim(
                            UUID.randomUUID(),
                            event.getTenantId(),
                            replayKey,
                            "VARIANT_PDF"
                    );

            if (claimed == 0) {

                log.debug(
                        "Duplicate PDF replay suppressed for output {}",
                        event.getOutputPath()
                );

                return;
            }

            if (event.getVariantIds() != null) {

                pdfService.generateSheetToFile(
                        event.getBranchId(),
                        event.getVariantIds(),
                        event.getOutputPath()
                );

            } else {

                List<UUID> ids =
                        variantRepo.findByProduct_IdSafe(
                                        event.getProductId(),
                                        false,
                                        event.getTenantId(),
                                        event.getBranchId()
                                ).stream()
                                .map(ProductVariant::getId)
                                .toList();

                pdfService.generateSheetToFile(
                        event.getBranchId(),
                        ids,
                        event.getOutputPath()
                );
            }

            log.info(
                    "PDF generated at {}",
                    event.getOutputPath()
            );

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
        }
    }
}