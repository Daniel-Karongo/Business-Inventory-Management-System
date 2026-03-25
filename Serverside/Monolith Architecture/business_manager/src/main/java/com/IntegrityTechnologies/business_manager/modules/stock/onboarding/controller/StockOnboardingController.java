package com.IntegrityTechnologies.business_manager.modules.stock.onboarding.controller;

import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.modules.stock.onboarding.dto.StockOnboardingRequest;
import com.IntegrityTechnologies.business_manager.modules.stock.onboarding.service.StockOnboardingService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/stock-onboarding")
@RequiredArgsConstructor
public class StockOnboardingController {

    private final StockOnboardingService service;

    @PostMapping
    public ApiResponse onboard(@RequestBody StockOnboardingRequest request) {

        return new ApiResponse(
                "success",
                "Stock onboarded",
                service.onboard(request)
        );
    }
}