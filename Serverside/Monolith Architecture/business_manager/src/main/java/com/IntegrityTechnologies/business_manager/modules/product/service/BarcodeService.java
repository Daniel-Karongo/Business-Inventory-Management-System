package com.IntegrityTechnologies.business_manager.modules.product.service;

import com.IntegrityTechnologies.business_manager.config.FileStorageService;
import com.google.zxing.BarcodeFormat;
import com.google.zxing.client.j2se.MatrixToImageWriter;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.oned.Code128Writer;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.pdfbox.pdmodel.*;
import org.apache.pdfbox.pdmodel.common.PDRectangle;
import org.apache.pdfbox.pdmodel.font.PDType1Font;
import org.apache.pdfbox.pdmodel.graphics.image.PDImageXObject;
import org.springframework.stereotype.Service;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.nio.file.*;
import java.util.List;
import java.util.UUID;

/**
 * Full-featured BarcodeService:
 * - Generates and validates barcodes
 * - Renders barcode PNGs
 * - Produces polished single-label and multi-label PDFs
 */
@Service
@Slf4j
@RequiredArgsConstructor
public class BarcodeService {

    private final FileStorageService fileStorageService;

    /* ================================
           BARCODE GENERATION / VALIDATION
           ================================ */
    public String autoGenerateBarcode() {
        return UUID.randomUUID().toString().replace("-", "").substring(0, 12).toUpperCase();
    }

    public boolean isValidBarcode(String barcode) {
        return barcode != null && barcode.matches("^[A-Z0-9]{8,20}$");
    }

    /* ================================
       IMAGE GENERATION
       ================================ */
    public String generateBarcodeImage(String barcode, Path outputDir) throws IOException {
        try {
            Files.createDirectories(outputDir);
            fileStorageService.hidePathIfSupported(outputDir);
            Path outputFile = outputDir.resolve(barcode + ".png");

            Code128Writer writer = new Code128Writer();
            BitMatrix bitMatrix = writer.encode(barcode, BarcodeFormat.CODE_128, 400, 120);
            MatrixToImageWriter.writeToPath(bitMatrix, "PNG", outputFile);

            System.out.println("Hello");
            System.out.println(outputFile);
            fileStorageService.hidePath(outputFile);
            System.out.println(outputFile);

            log.info("✅ Barcode image generated at {}", outputFile);
            return outputFile.toString();
        } catch (Exception e) {  // catch general Exception to include runtime encoding issues
            throw new IOException("Failed to generate barcode image", e);
        }
    }

    private BufferedImage createBarcodeImage(String barcode) throws IOException {
        try {
            Code128Writer writer = new Code128Writer();
            BitMatrix matrix = writer.encode(barcode, BarcodeFormat.CODE_128, 400, 120);
            return MatrixToImageWriter.toBufferedImage(matrix);
        } catch (Exception e) {
            throw new IOException("Barcode image creation failed", e);
        }
    }

    /* ================================
       SINGLE PRODUCT PDF LABEL
       ================================ */
    public String generateBarcodeLabelPDF(String barcode, String productName, Path outputDir) throws IOException {
        Files.createDirectories(outputDir);
        Path pdfPath = outputDir.resolve(barcode + "_label.pdf");

        BufferedImage barcodeImage = createBarcodeImage(barcode);

        try (PDDocument doc = new PDDocument()) {
            PDPage page = new PDPage(PDRectangle.A6);
            doc.addPage(page);

            try (PDPageContentStream content = new PDPageContentStream(doc, page)) {
                // Header
                content.beginText();
                content.setFont(PDType1Font.HELVETICA_BOLD, 16);
                content.newLineAtOffset(50, page.getMediaBox().getHeight() - 50);
                content.showText("Product Barcode Label");
                content.endText();

                // Product name
                content.beginText();
                content.setFont(PDType1Font.HELVETICA, 14);
                content.newLineAtOffset(50, page.getMediaBox().getHeight() - 80);
                content.showText("Product: " + productName);
                content.endText();

                // SKU / Barcode text
                content.beginText();
                content.setFont(PDType1Font.HELVETICA_OBLIQUE, 12);
                content.newLineAtOffset(50, page.getMediaBox().getHeight() - 100);
                content.showText("SKU: " + barcode);
                content.endText();

                // Barcode image
                Path temp = Files.createTempFile("barcode_", ".png");
                ImageIO.write(barcodeImage, "PNG", temp.toFile());
                PDImageXObject image = PDImageXObject.createFromFile(temp.toString(), doc);
                content.drawImage(image, 50, 60, 250, 70);
                Files.deleteIfExists(temp);

                // Footer
                content.beginText();
                content.setFont(PDType1Font.HELVETICA, 10);
                content.newLineAtOffset(50, 40);
                content.showText("Generated by Integrity Business Manager");
                content.endText();
            }

            doc.save(pdfPath.toFile());
        }

        log.info("📦 Barcode PDF created: {}", pdfPath);
        return pdfPath.toString();
    }

    /* ================================
       MULTI-PRODUCT SHEET PDF
       ================================ */
    public String generateBarcodeSheetPDF(List<?> products, Path outputDir) throws IOException {
        Files.createDirectories(outputDir);
        Path pdfPath = outputDir.resolve("barcode_sheet_" + System.currentTimeMillis() + ".pdf");

        try (PDDocument doc = new PDDocument()) {
            PDPage page = new PDPage(PDRectangle.LETTER);
            PDPageContentStream content = new PDPageContentStream(doc, page);

            float margin = 40;
            float y = page.getMediaBox().getHeight() - margin;
            float spacing = 100;

            for (Object obj : products) {
                String barcode = (String) obj.getClass().getMethod("getBarcode").invoke(obj);
                String name = (String) obj.getClass().getMethod("getName").invoke(obj);

                BufferedImage image = createBarcodeImage(barcode);
                Path temp = Files.createTempFile("barcode_", ".png");
                ImageIO.write(image, "PNG", temp.toFile());
                PDImageXObject pdImage = PDImageXObject.createFromFile(temp.toString(), doc);

                if (y < 150) {
                    content.close();
                    page = new PDPage(PDRectangle.LETTER);
                    doc.addPage(page);
                    content = new PDPageContentStream(doc, page);
                    y = page.getMediaBox().getHeight() - margin;
                }

                content.beginText();
                content.setFont(PDType1Font.HELVETICA_BOLD, 12);
                content.newLineAtOffset(margin, y);
                content.showText(name + " (" + barcode + ")");
                content.endText();

                content.drawImage(pdImage, margin, y - 70, 250, 60);
                Files.deleteIfExists(temp);
                y -= spacing;
            }

            content.close();
            doc.addPage(page);
            doc.save(pdfPath.toFile());
        } catch (Exception e) {
            throw new IOException("Failed to generate barcode sheet PDF", e);
        }

        return pdfPath.toString();
    }
}