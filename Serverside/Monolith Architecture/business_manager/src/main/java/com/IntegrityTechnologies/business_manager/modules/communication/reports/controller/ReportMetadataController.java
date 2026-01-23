package com.IntegrityTechnologies.business_manager.modules.communication.reports.controller;

import com.IntegrityTechnologies.business_manager.modules.communication.reports.dto.*;
import com.IntegrityTechnologies.business_manager.modules.communication.reports.registry.ReportDefinition;
import com.IntegrityTechnologies.business_manager.modules.communication.reports.registry.ReportRegistry;
import com.IntegrityTechnologies.business_manager.modules.communication.reports.security.ReportAccess;
import com.IntegrityTechnologies.business_manager.modules.communication.reports.security.ReportLimitRegistry;
import com.IntegrityTechnologies.business_manager.modules.communication.reports.security.ReportLimits;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.*;

@RestController
@RequestMapping("/api/reports")
@RequiredArgsConstructor
public class ReportMetadataController {

    @GetMapping("/definitions")
    public List<ReportMetadataDto> definitions() {

        var role = SecurityUtils.currentRole();

        List<ReportMetadataDto> out = new ArrayList<>();

        for (ReportDefinition def : ReportRegistry.REPORTS.values()) {

            ReportAccess access = ReportAccess.fromReportName(def.key());
            if (!access.canAccess(role)) continue;

            ReportLimits limits = ReportLimitRegistry.get(def.key());

            out.add(
                    ReportMetadataDto.builder()
                            .key(def.key())
                            .title(def.title())
                            .description(def.description())
                            .category(def.category())
                            .parameters(buildParams(def.requiredParams()))
                            .minimumRole(access.getMinimumRole().name())
                            .supportsPdf(true)
                            .supportsXlsx(true)
                            .supportsCsv(true)
                            .maxDateRange(limits != null ? limits.maxRange() : null)
                            .dateRangeUnit(limits != null ? limits.unit().name() : null)
                            .build()
            );
        }

        return out;
    }

    private List<ReportParameterDto> buildParams(Set<String> required) {

        List<ReportParameterDto> params = new ArrayList<>();

        for (String p : required) {
            params.add(
                    new ReportParameterDto(
                            p,
                            true,
                            inferType(p)
                    )
            );
        }

        return params;
    }

    private String inferType(String param) {

        // ---- DATES ----
        if (param.endsWith("_DATE")) return "DATE";

        // ---- ENTITIES (specific first!) ----
        if (param.contains("BRANCH")) return "BRANCH";
        if (param.contains("ACCOUNT")) return "ACCOUNT";
        if (param.contains("CUSTOMER")) return "CUSTOMER";
        if (param.contains("SUPPLIER")) return "SUPPLIER";
        if (param.contains("PRODUCT")) return "PRODUCT";
        if (param.contains("VARIANT")) return "VARIANT";
        if (param.contains("CATEGORY")) return "CATEGORY";

        // ---- NUMERIC ----
        if (param.contains("THRESHOLD")) return "NUMBER";

        // ---- FALLBACK ----
        return "TEXT";
    }
}