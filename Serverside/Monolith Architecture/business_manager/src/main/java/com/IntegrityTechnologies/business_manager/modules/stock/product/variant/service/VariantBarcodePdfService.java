package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.service;

import com.IntegrityTechnologies.business_manager.config.files.FileStorageService;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchContext;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.PDPageContentStream;
import org.apache.pdfbox.pdmodel.common.PDRectangle;
import org.apache.pdfbox.pdmodel.font.PDType1Font;
import org.apache.pdfbox.pdmodel.graphics.image.PDImageXObject;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class VariantBarcodePdfService {

    private final ProductVariantRepository variantRepo;
    private final VariantBarcodeService barcodeService;
    private final FileStorageService fileStorageService;

    private UUID tenantId() { return TenantContext.getTenantId(); }
    private UUID branchId() { return BranchContext.get(); }

    /* ============================================================
       SAFE LOAD
       ============================================================ */

    private ProductVariant getVariant(UUID id) {
        return variantRepo.findByIdSafe(
                id,
                false,
                tenantId(),
                branchId()
        ).orElseThrow(() -> new EntityNotFoundException("Variant not found"));
    }

    private List<ProductVariant> getVariantsForProduct(UUID productId) {
        return variantRepo.findByProduct_IdSafe(
                productId,
                false,
                tenantId(),
                branchId()
        );
    }

    /* ============================================================
       PATH RESOLUTION (CRITICAL)
       ============================================================ */

    private Path resolveBarcodePath(ProductVariant variant) {

        return fileStorageService.productRoot()
                .resolve(variant.getProduct().getId().toString())
                .resolve("variants")
                .resolve(variant.getId().toString())
                .resolve("barcode")
                .resolve(variant.getBarcode() + ".png");
    }

    /* ============================================================
       SINGLE LABEL
       ============================================================ */

    public File generateSingleLabel(UUID variantId) throws IOException {

        ProductVariant variant = getVariant(variantId);

        barcodeService.ensureBarcodeExists(variant);

        Path imagePath = resolveBarcodePath(variant);

        if (!imagePath.toFile().exists()) {
            throw new EntityNotFoundException("Barcode image missing");
        }

        File pdf = File.createTempFile(
                "variant-barcode-" + variant.getId(),
                ".pdf"
        );

        try (PDDocument doc = new PDDocument()) {

            PDPage page = new PDPage(PDRectangle.A6);
            doc.addPage(page);

            PDImageXObject barcodeImage =
                    PDImageXObject.createFromFile(imagePath.toString(), doc);

            try (PDPageContentStream cs = new PDPageContentStream(doc, page)) {

                float top = page.getMediaBox().getHeight();

                cs.beginText();
                cs.setFont(PDType1Font.HELVETICA_BOLD, 14);
                cs.newLineAtOffset(30, top - 30);
                cs.showText("Product Barcode Label");
                cs.endText();

                cs.beginText();
                cs.setFont(PDType1Font.HELVETICA, 12);
                cs.newLineAtOffset(30, top - 55);
                cs.showText("Product: " + variant.getProduct().getName());
                cs.endText();

                cs.beginText();
                cs.setFont(PDType1Font.HELVETICA_OBLIQUE, 11);
                cs.newLineAtOffset(30, top - 75);
                cs.showText("Variant: " + variant.getClassification());
                cs.endText();

                cs.drawImage(barcodeImage, 30, top - 160, 220, 60);

                cs.beginText();
                cs.setFont(PDType1Font.HELVETICA, 10);
                cs.newLineAtOffset(30, top - 175);
                cs.showText("Barcode: " + variant.getBarcode());
                cs.endText();
            }

            doc.save(pdf);
        }

        return pdf;
    }

    /* ============================================================
       MULTI SHEET
       ============================================================ */

    public Path generateSheetToFile(List<UUID> variantIds, String outputPath) throws IOException {

        List<ProductVariant> variants = variantIds.stream()
                .map(this::getVariant)
                .toList();

        Path output = Path.of(outputPath);

        try (PDDocument doc = new PDDocument()) {

            PDPage page = new PDPage(PDRectangle.LETTER);
            doc.addPage(page);

            PDPageContentStream cs = new PDPageContentStream(doc, page);

            float margin = 40;
            float y = page.getMediaBox().getHeight() - margin;
            float spacing = 95;

            for (ProductVariant variant : variants) {

                barcodeService.ensureBarcodeExists(variant);

                Path imagePath = resolveBarcodePath(variant);

                PDImageXObject barcodeImage =
                        PDImageXObject.createFromFile(imagePath.toString(), doc);

                if (y < 130) {
                    cs.close();
                    page = new PDPage(PDRectangle.LETTER);
                    doc.addPage(page);
                    cs = new PDPageContentStream(doc, page);
                    y = page.getMediaBox().getHeight() - margin;
                }

                cs.beginText();
                cs.setFont(PDType1Font.HELVETICA_BOLD, 11);
                cs.newLineAtOffset(margin, y);
                cs.showText(variant.getProduct().getName() + " — " + variant.getClassification());
                cs.endText();

                cs.drawImage(barcodeImage, margin, y - 60, 240, 55);

                cs.beginText();
                cs.setFont(PDType1Font.HELVETICA, 9);
                cs.newLineAtOffset(margin, y - 72);
                cs.showText(variant.getBarcode());
                cs.endText();

                y -= spacing;
            }

            cs.close();
            doc.save(output.toFile());
        }

        return output;
    }
}