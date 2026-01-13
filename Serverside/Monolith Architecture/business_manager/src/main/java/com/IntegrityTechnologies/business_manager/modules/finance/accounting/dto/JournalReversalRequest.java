package com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto;

import jakarta.validation.constraints.NotBlank;

public record JournalReversalRequest(
        @NotBlank String reason
) {}