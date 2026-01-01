package com.IntegrityTechnologies.business_manager.modules.communication.reports.dto;

import lombok.Data;

import java.util.Map;

@Data
public class ReportRequest {
    private String reportName;     // e.g. "trial_balance"
    private Map<String, Object> parameters;
}