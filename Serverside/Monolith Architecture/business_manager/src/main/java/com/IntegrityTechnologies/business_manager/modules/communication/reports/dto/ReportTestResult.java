package com.IntegrityTechnologies.business_manager.modules.communication.reports.dto;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class ReportTestResult {
    private String reportName;
    private boolean success;
    private String message;
}