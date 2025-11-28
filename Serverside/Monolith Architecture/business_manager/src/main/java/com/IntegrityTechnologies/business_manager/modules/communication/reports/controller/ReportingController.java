package com.IntegrityTechnologies.business_manager.modules.communication.reports.controller;

import com.IntegrityTechnologies.business_manager.modules.communication.reports.service.ReportingService;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.io.ByteArrayOutputStream;
import java.time.LocalDate;
import java.util.UUID;

@Tag(name = "Reports")
@RestController
@RequestMapping("/api/reports")
@RequiredArgsConstructor
public class ReportingController {

    private final ReportingService reportingService;

    @GetMapping(value = "/sales-summary", produces = MediaType.APPLICATION_PDF_VALUE)
    public ResponseEntity<byte[]> salesSummaryPdf(
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to
    ) throws Exception {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        reportingService.generateSalesSummaryPdf(from, to, baos);
        byte[] bytes = baos.toByteArray();

        return ResponseEntity.ok()
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=sales_summary_" + from + "_to_" + to + ".pdf")
                .contentType(MediaType.APPLICATION_PDF)
                .body(bytes);
    }

    @GetMapping(value = "/receipt/{saleId}", produces = MediaType.APPLICATION_PDF_VALUE)
    public ResponseEntity<byte[]> saleReceiptPdf(@PathVariable UUID saleId) throws Exception {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        reportingService.generateSaleReceiptPdf(saleId, baos);
        byte[] bytes = baos.toByteArray();

        return ResponseEntity.ok()
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=receipt_" + saleId + ".pdf")
                .contentType(MediaType.APPLICATION_PDF)
                .body(bytes);
    }

    @GetMapping(value = "/purchase-order/{poId}", produces = MediaType.APPLICATION_PDF_VALUE)
    public ResponseEntity<byte[]> purchaseOrderPdf(@PathVariable UUID poId) throws Exception {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        reportingService.generatePurchaseOrderPdf(poId, baos);
        byte[] bytes = baos.toByteArray();

        return ResponseEntity.ok()
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=po_" + poId + ".pdf")
                .contentType(MediaType.APPLICATION_PDF)
                .body(bytes);
    }
}