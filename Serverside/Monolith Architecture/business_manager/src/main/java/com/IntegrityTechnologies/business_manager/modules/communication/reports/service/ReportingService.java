package com.IntegrityTechnologies.business_manager.modules.communication.reports.service;

import com.IntegrityTechnologies.business_manager.modules.communication.reports.dto.ReportRequest;

import java.io.OutputStream;

public interface ReportingService {
    void generate(ReportRequest request, OutputStream out) throws Exception;
}