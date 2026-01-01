package com.IntegrityTechnologies.business_manager.modules.communication.reports.controller;

import com.IntegrityTechnologies.business_manager.modules.communication.reports.dto.ReportRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.reports.service.ReportingService;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.io.ByteArrayOutputStream;

@Tag(name = "Reports")
@RestController
@RequestMapping("/api/reports")
@RequiredArgsConstructor
public class ReportController {

    private final ReportingService reportingService;

    @PostMapping(value = "/generate", produces = MediaType.APPLICATION_PDF_VALUE)
    public ResponseEntity<byte[]> generate(@RequestBody ReportRequest request) throws Exception {

        ByteArrayOutputStream out = new ByteArrayOutputStream();
        reportingService.generate(request, out);

        return ResponseEntity.ok()
                .header(
                        HttpHeaders.CONTENT_DISPOSITION,
                        "attachment; filename=" + request.getReportName() + ".pdf"
                )
                .contentType(MediaType.APPLICATION_PDF)
                .body(out.toByteArray());
    }
}