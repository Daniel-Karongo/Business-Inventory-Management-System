package com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Data
public class ReverseAllocationRequest {

    @NotBlank
    private String reason;
}