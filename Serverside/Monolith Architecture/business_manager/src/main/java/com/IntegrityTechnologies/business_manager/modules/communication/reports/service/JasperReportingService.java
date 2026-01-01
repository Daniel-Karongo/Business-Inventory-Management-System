package com.IntegrityTechnologies.business_manager.modules.communication.reports.service;

import com.IntegrityTechnologies.business_manager.modules.communication.reports.dto.ReportRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.reports.registry.ReportRegistry;
import lombok.RequiredArgsConstructor;
import net.sf.jasperreports.engine.*;
import net.sf.jasperreports.engine.data.JRBeanCollectionDataSource;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Service;

import javax.sql.DataSource;
import java.io.InputStream;
import java.io.OutputStream;
import java.sql.Connection;
import java.util.Collections;
import java.util.Map;

@Service
@RequiredArgsConstructor
public class JasperReportingService implements ReportingService {

    private final DataSource dataSource;

    @Override
    public void generate(ReportRequest request, OutputStream out) throws Exception {

        String jrxmlPath = ReportRegistry.REPORTS.get(request.getReportName());
        if (jrxmlPath == null) {
            throw new IllegalArgumentException("Unknown report: " + request.getReportName());
        }

        try (
                InputStream jrxml = new ClassPathResource(jrxmlPath).getInputStream();
                Connection conn = dataSource.getConnection()
        ) {

            JasperReport report = JasperCompileManager.compileReport(jrxml);

            Map<String, Object> params =
                    request.getParameters() != null ? request.getParameters() : Map.of();

            JasperPrint jp = JasperFillManager.fillReport(report, params, conn);
            JasperExportManager.exportReportToPdfStream(jp, out);
        }
    }
}