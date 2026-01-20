package com.IntegrityTechnologies.business_manager.modules.communication.notification.email.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import lombok.Data;

import java.util.List;

@Data
public class EmailRequest {

    @NotEmpty
    private List<String> to;

    @NotBlank
    private String subject;

    @NotBlank
    private String body;

    /**
     * Audit / traceability
     */
    private String createdBy;
}