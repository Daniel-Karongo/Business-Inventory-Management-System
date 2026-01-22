package com.IntegrityTechnologies.business_manager.modules.communication.reports.service;

import com.IntegrityTechnologies.business_manager.modules.communication.reports.dto.ReportRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.reports.dto.ReportTestResult;

import java.io.OutputStream;
import java.util.List;

public interface ReportingService {
    void generate(ReportRequest request, OutputStream out) throws Exception;

    List<ReportTestResult> testAllReports();
}
