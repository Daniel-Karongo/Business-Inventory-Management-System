package com.IntegrityTechnologies.business_manager.modules.dashboard.controller;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import com.IntegrityTechnologies.business_manager.modules.dashboard.service.DashboardService;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@Tag(name = "Dashboard")
@RestController
@RequestMapping("/api/dashboard")
@RequiredArgsConstructor
public class DashboardController {

    private final DashboardService dashboardService;

    @GetMapping("/summary")
    public ResponseEntity<ApiResponse> summary(
            @RequestParam UUID branchId
    ) {
        return ResponseEntity.ok(
                new ApiResponse(
                        "success",
                        "Branch dashboard summary",
                        dashboardService.getBranchDashboard(branchId)
                )
        );
    }
}