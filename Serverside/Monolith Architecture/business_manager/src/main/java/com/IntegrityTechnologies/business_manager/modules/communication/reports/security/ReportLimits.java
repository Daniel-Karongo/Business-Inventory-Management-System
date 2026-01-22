package com.IntegrityTechnologies.business_manager.modules.communication.reports.security;

import java.time.temporal.ChronoUnit;

public record ReportLimits(
        long maxRange,
        ChronoUnit unit
) {}