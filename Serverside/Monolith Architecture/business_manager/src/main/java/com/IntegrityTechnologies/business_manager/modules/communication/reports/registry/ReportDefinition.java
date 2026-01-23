package com.IntegrityTechnologies.business_manager.modules.communication.reports.registry;

import java.util.Set;

public record ReportDefinition(
        String key,
        String title,
        String description,
        String category,
        String jrxmlPath,
        Set<String> requiredParams
) {}