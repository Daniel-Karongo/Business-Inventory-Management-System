package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.service.PeriodClosingService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.UUID;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/accounting/periods")
public class PeriodController {

    private final PeriodClosingService closingService;

    @PostMapping("/{id}/close")
    public ResponseEntity<?> close(
            @PathVariable UUID id
    ) {
        closingService.closePeriod(id, "SYSTEM");
        return ResponseEntity.ok("Period closed successfully");
    }
}