package com.IntegrityTechnologies.business_manager.modules.stock.onboarding.controller;

import com.IntegrityTechnologies.business_manager.config.bulk.BulkRequest;
import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.modules.stock.onboarding.dto.BulkStockOnboardingRequest;
import com.IntegrityTechnologies.business_manager.modules.stock.onboarding.service.StockOnboardingBulkService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/stock/onboarding/bulk")
@RequiredArgsConstructor
public class StockOnboardingBulkController {

    private final StockOnboardingBulkService bulkService;

    @PostMapping
    public ApiResponse bulkOnboard(
            @RequestBody
            BulkRequest<BulkStockOnboardingRequest> request
    ) {
        return new ApiResponse(
                "success",
                "Bulk onboarding processed",
                bulkService.bulkOnboard(request)
        );
    }
}