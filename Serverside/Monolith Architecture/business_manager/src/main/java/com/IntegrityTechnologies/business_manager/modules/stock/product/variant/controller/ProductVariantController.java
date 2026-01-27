package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.controller;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.service.*;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/product-variants")
@RequiredArgsConstructor
@Tag(name = "Product Variants")
public class ProductVariantController {

    private final ProductVariantService service;
    private final VariantBarcodeService barcodeService;
    private final ProductVariantImageService imageService;
    private final VariantBarcodePdfService pdfService;
    private final BarcodeScanService scanService;

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @PostMapping
    public ResponseEntity<ProductVariantDTO> create(@RequestBody ProductVariantCreateDTO dto) {
        return ResponseEntity.ok(service.createVariant(dto));
    }

    @GetMapping("/{id}")
    public ResponseEntity<ProductVariantDTO> find(@PathVariable UUID id) {
        return ResponseEntity.ok(service.getVariant(id));
    }

    @GetMapping("/product/{productId}")
    public ResponseEntity<List<ProductVariantDTO>> findForProduct(@PathVariable UUID productId) {
        return ResponseEntity.ok(service.getVariantsForProduct(productId));
    }

    @GetMapping("/barcode/{barcode}")
    public ResponseEntity<ProductVariantDTO> findByBarcode(@PathVariable String barcode) {
        return ResponseEntity.ok(barcodeService.getVariantByBarcode(barcode));
    }

    @PostMapping("/scan")
    public ResponseEntity<BarcodeScanResponse> scan(
            @RequestBody BarcodeScanRequest req
    ) {
        return ResponseEntity.ok(
                scanService.scan(req.getBarcode(), req.getBranchId())
        );
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @PostMapping("/{id}/barcode")
    public ResponseEntity<ProductVariantDTO> generateBarcode(@PathVariable UUID id) {
        return ResponseEntity.ok(barcodeService.generateBarcodeIfMissing(id));
    }

    @GetMapping("/{id}/barcode/image")
    public ResponseEntity<Resource> downloadBarcodeImage(@PathVariable UUID id) {
        ProductVariant v = service.getEntity(id);

        if (v.getBarcodeImagePath() == null)
            throw new IllegalStateException("No barcode image for variant");

        File file = new File(v.getBarcodeImagePath());
        Resource res = new FileSystemResource(file);

        return ResponseEntity.ok()
                .contentType(MediaType.IMAGE_PNG)
                .header(HttpHeaders.CONTENT_DISPOSITION,
                        "attachment; filename=barcode-" + v.getSku() + ".png")
                .body(res);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @PostMapping("/{id}/images")
    public ResponseEntity<Void> uploadImages(
            @PathVariable UUID id,
            @RequestParam("files") List<MultipartFile> files
    ) throws IOException {
        imageService.uploadVariantImages(id, files);
        return ResponseEntity.noContent().build();
    }

    @GetMapping("/{id}/images")
    public ResponseEntity<List<String>> getImages(@PathVariable UUID id) {
        return ResponseEntity.ok(imageService.getImageUrls(id));
    }

    @GetMapping("/{id}/barcode/pdf")
    public ResponseEntity<Resource> barcodePdf(@PathVariable UUID id) throws IOException {

        File pdf = pdfService.generatePdf(
                List.of(service.getEntity(id))
        );

        return pdfResponse(pdf, "variant-" + id + "-barcode.pdf");
    }

    @PostMapping("/barcode/pdf/bulk")
    public ResponseEntity<Resource> bulkPdf(@RequestBody List<UUID> variantIds) throws IOException {

        List<ProductVariant> variants =
                variantIds.stream().map(service::getEntity).toList();

        File pdf = pdfService.generatePdf(variants);

        return pdfResponse(pdf, "variant-barcodes.pdf");
    }

    @GetMapping("/product/{productId}/barcode/pdf")
    public ResponseEntity<Resource> productPdf(@PathVariable UUID productId) throws IOException {

        List<ProductVariant> variants =
                service.getEntitiesForProduct(productId);

        File pdf = pdfService.generatePdf(variants);

        return pdfResponse(pdf, "product-" + productId + "-barcodes.pdf");
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @PutMapping("/{id}")
    public ResponseEntity<ProductVariantDTO> update(
            @PathVariable UUID id,
            @RequestBody ProductVariantUpdateDTO dto
    ) {
        return ResponseEntity.ok(service.updateVariant(id, dto));
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN')")
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> delete(@PathVariable UUID id) {
        service.deleteVariant(id);
        return ResponseEntity.noContent().build();
    }

    private ResponseEntity<Resource> pdfResponse(File file, String name) {
        Resource res = new FileSystemResource(file);
        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_PDF)
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + name)
                .body(res);
    }
}