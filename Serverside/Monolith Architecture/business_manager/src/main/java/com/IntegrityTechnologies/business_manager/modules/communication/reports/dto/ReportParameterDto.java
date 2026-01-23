package com.IntegrityTechnologies.business_manager.modules.communication.reports.dto;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class ReportParameterDto {
    private String name;
    private boolean required;
    private String type; // DATE, BRANCH, ACCOUNT, CUSTOMER, SUPPLIER, NUMBER
}