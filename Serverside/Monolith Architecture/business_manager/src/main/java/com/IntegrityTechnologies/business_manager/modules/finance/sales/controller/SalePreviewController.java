package com.IntegrityTechnologies.business_manager.modules.finance.sales.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.dto.SaleLinePreviewRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.dto.SaleLinePreviewResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.service.SalePreviewService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/sales")
@RequiredArgsConstructor
public class SalePreviewController {

    private final SalePreviewService previewService;

    @PostMapping("/preview-line")
    public SaleLinePreviewResponse preview(@RequestBody SaleLinePreviewRequest req) {
        return previewService.preview(req);
    }
}