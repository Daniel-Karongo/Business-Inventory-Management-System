package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.service;

import com.IntegrityTechnologies.business_manager.config.files.FileStorageService;
import com.IntegrityTechnologies.business_manager.config.kafka.OutboxEventWriter;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.events.VariantBarcodePdfRequestedEvent;
import com.IntegrityTechnologies.business_manager.security.util.BranchContext;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class VariantPdfOrchestrationService {

    private final FileStorageService fileStorageService;
    private final OutboxEventWriter outboxEventWriter;

    public String requestBulkPdf(List<UUID> variantIds) {

        if (variantIds.size() > 200) {
            throw new IllegalArgumentException("Maximum 200 variants allowed per request");
        }

        String fileName = "barcode-" + UUID.randomUUID() + ".pdf";

        Path dir = fileStorageService.initDirectory(
                fileStorageService.productSharedRoot().resolve("_pdf")
        );

        Path output = dir.resolve(fileName);

        outboxEventWriter.write(
                "VARIANT_BARCODE_PDF_REQUESTED",
                BranchContext.get(),
                VariantBarcodePdfRequestedEvent.builder()
                        .tenantId(TenantContext.getTenantId())
                        .branchId(BranchContext.get())
                        .variantIds(variantIds)
                        .outputPath(output.toString())
                        .build()
        );

        return fileName;
    }

    public String requestProductPdf(UUID productId) {

        String fileName = "barcode-product-" + UUID.randomUUID() + ".pdf";

        Path dir = fileStorageService.initDirectory(
                fileStorageService.productSharedRoot().resolve("_pdf")
        );

        Path output = dir.resolve(fileName);

        outboxEventWriter.write(
                "VARIANT_BARCODE_PDF_REQUESTED",
                BranchContext.get(),
                VariantBarcodePdfRequestedEvent.builder()
                        .tenantId(TenantContext.getTenantId())
                        .branchId(BranchContext.get())
                        .productId(productId)
                        .outputPath(output.toString())
                        .build()
        );

        return fileName;
    }

    public Resource getPdfResource(String fileName) {

        Path path = fileStorageService.productSharedRoot()
                .resolve("_pdf")
                .resolve(fileName);

        if (!Files.exists(path)) {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND);
        }

        return new FileSystemResource(path);
    }
}