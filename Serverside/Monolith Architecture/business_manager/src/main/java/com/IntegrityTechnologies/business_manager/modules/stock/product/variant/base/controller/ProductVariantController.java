package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantAdminOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.ProductVariantCreateDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.ProductVariantDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.ProductVariantUpdateDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.VariantAuditDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.service.ProductVariantImageService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.service.ProductVariantService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.service.VariantPdfOrchestrationService;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/product-variants")
@RequiredArgsConstructor
@Tag(name = "Product Variants")
@TenantUserOnly
public class ProductVariantController {

    private final ProductVariantService service;
    private final ProductVariantImageService imageService;
    private final VariantPdfOrchestrationService pdfOrchestrationService;

    @TenantManagerOnly
    @PostMapping
    public ResponseEntity<ProductVariantDTO> create(
            @RequestParam UUID branchId,
            @RequestBody ProductVariantCreateDTO dto
    ) {
        return ResponseEntity.ok(
                service.createVariant(branchId, dto)
        );
    }

    @GetMapping("/{id}")
    public ResponseEntity<ProductVariantDTO> find(
            @RequestParam UUID branchId,
            @PathVariable UUID id
    ) {
        return ResponseEntity.ok(
                service.getVariant(branchId, id)
        );
    }

    @GetMapping("/product/{productId}")
    public ResponseEntity<List<ProductVariantDTO>> findForProduct(
            @RequestParam UUID branchId,
            @PathVariable UUID productId
    ) {
        return ResponseEntity.ok(
                service.getVariantsForProduct(branchId, productId)
        );
    }

    @GetMapping("/{id}/images")
    public ResponseEntity<List<String>> getImages(
            @RequestParam UUID branchId,
            @PathVariable UUID id
    ) {
        return ResponseEntity.ok(
                imageService.getImageUrls(branchId, id)
        );
    }

    @PostMapping("/barcode/pdf/bulk")
    public ResponseEntity<String> bulkPdf(
            @RequestParam UUID branchId,
            @RequestBody List<UUID> variantIds
    ) {
        String fileName =
                pdfOrchestrationService.requestBulkPdf(
                        branchId,
                        variantIds
                );

        return ResponseEntity.accepted().body(fileName);
    }

    @GetMapping("/barcode/pdf/download/{fileName}")
    public ResponseEntity<Resource> downloadPdf(
            @PathVariable String fileName,
            @RequestParam UUID branchId
    ) {

        Resource resource =
                pdfOrchestrationService.getPdfResource(
                        branchId,
                        fileName
                );

        return ResponseEntity.ok()
                .header(
                        HttpHeaders.CONTENT_DISPOSITION,
                        "attachment; filename=" + fileName
                )
                .body(resource);
    }

    @GetMapping("/product/{productId}/barcode/pdf")
    public ResponseEntity<String> productPdf(
            @RequestParam UUID branchId,
            @PathVariable UUID productId
    ) {

        String fileName =
                pdfOrchestrationService.requestProductPdf(
                        branchId,
                        productId
                );

        return ResponseEntity.accepted().body(fileName);
    }

    @TenantManagerOnly
    @PutMapping("/{id}")
    public ResponseEntity<ProductVariantDTO> update(
            @RequestParam UUID branchId,
            @PathVariable UUID id,
            @RequestParam(required = false) String reason,
            @RequestBody ProductVariantUpdateDTO dto
    ) {
        return ResponseEntity.ok(
                service.updateVariant(branchId, id, dto, reason)
        );
    }

    @TenantAdminOnly
    @PostMapping("/{id}/restore")
    public ResponseEntity<Void> restore(
            @RequestParam UUID branchId,
            @RequestParam(required = false) String reason,
            @PathVariable UUID id
    ) {
        service.restoreVariant(
                branchId,
                id,
                reason
        );

        return ResponseEntity.noContent().build();
    }

    @TenantAdminOnly
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> delete(
            @RequestParam UUID branchId,
            @RequestParam(required = false) String reason,
            @PathVariable UUID id
    ) {

        service.deleteVariant(
                branchId,
                id,
                reason
        );

        return ResponseEntity.noContent().build();
    }

    @TenantAdminOnly
    @DeleteMapping("/{id}/hard")
    public ResponseEntity<Void> hardDelete(
            @RequestParam UUID branchId,
            @RequestParam(required = false) String reason,
            @PathVariable UUID id
    ) {

        service.hardDeleteVariant(
                branchId,
                id,
                reason
        );

        return ResponseEntity.noContent().build();
    }

    @GetMapping("/{id}/audits")
    public ResponseEntity<List<VariantAuditDTO>> getAuditHistory(
            @RequestParam UUID branchId,
            @PathVariable UUID id
    ) {
        return ResponseEntity.ok(
                service.getAuditHistory(
                        branchId,
                        id
                )
        );
    }
}