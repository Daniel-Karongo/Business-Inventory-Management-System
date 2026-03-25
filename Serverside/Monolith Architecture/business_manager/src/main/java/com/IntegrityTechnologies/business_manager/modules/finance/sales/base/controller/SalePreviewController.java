package com.IntegrityTechnologies.business_manager.modules.finance.sales.base.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.dto.SaleLinePreviewRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.dto.SaleLinePreviewResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.service.SalePreviewService;
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