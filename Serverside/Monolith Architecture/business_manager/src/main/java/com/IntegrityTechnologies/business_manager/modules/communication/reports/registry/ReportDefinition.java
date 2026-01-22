package com.IntegrityTechnologies.business_manager.modules.communication.reports.registry;

import java.util.Set;

public record ReportDefinition(
        String key,
        String jrxmlPath,
        Set<String> requiredParams
) {}