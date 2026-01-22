package com.IntegrityTechnologies.business_manager.modules.communication.reports.controller;

import com.IntegrityTechnologies.business_manager.modules.communication.reports.dto.ReportFormat;
import com.IntegrityTechnologies.business_manager.modules.communication.reports.dto.ReportRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.reports.dto.ReportTestResult;
import com.IntegrityTechnologies.business_manager.modules.communication.reports.service.ReportingService;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.io.ByteArrayOutputStream;
import java.util.List;

@Tag(name = "Reports")
@RestController
@RequestMapping("/api/reports")
@RequiredArgsConstructor
public class ReportController {

    private final ReportingService reportingService;

    @PostMapping("/generate")
    public ResponseEntity<byte[]> generate(@RequestBody ReportRequest request) throws Exception {

        ByteArrayOutputStream out = new ByteArrayOutputStream();
        reportingService.generate(request, out);

        String filename =
                request.getReportName() + "." +
                        (request.getFormat() != null
                                ? request.getFormat().name().toLowerCase()
                                : "pdf");

        MediaType mediaType =
                switch (request.getFormat() != null ? request.getFormat() : ReportFormat.PDF) {
                    case XLSX -> MediaType.APPLICATION_OCTET_STREAM;
                    case CSV -> MediaType.TEXT_PLAIN;
                    default -> MediaType.APPLICATION_PDF;
                };

        return ResponseEntity.ok()
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + filename)
                .contentType(mediaType)
                .body(out.toByteArray());
    }

    @GetMapping("/test")
    public ResponseEntity<List<ReportTestResult>> testAll() {

        return ResponseEntity.ok(
                reportingService.testAllReports()
        );
    }

}