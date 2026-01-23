package com.IntegrityTechnologies.business_manager.modules.communication.reports.dto;

import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class ReportMetadataDto {

    private String key;
    private String title;
    private String description;
    private String category;

    private List<ReportParameterDto> parameters;

    private String minimumRole;

    private boolean supportsPdf;
    private boolean supportsXlsx;
    private boolean supportsCsv;

    private Long maxDateRange;
    private String dateRangeUnit;
}