package com.IntegrityTechnologies.business_manager.modules.stock.onboarding.dto;

import com.IntegrityTechnologies.business_manager.config.bulk.BulkError;
import lombok.*;

import java.util.ArrayList;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class StockOnboardingRowValidationResult {

    private int row;

    private boolean valid;

    @Builder.Default
    private List<BulkError> errors =
            new ArrayList<>();

    private StockOnboardingBulkPreviewRow preview;
}