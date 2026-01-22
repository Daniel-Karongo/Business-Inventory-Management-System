package com.IntegrityTechnologies.business_manager.modules.communication.reports.service;

import com.IntegrityTechnologies.business_manager.modules.communication.reports.dto.ReportFormat;
import com.IntegrityTechnologies.business_manager.modules.communication.reports.dto.ReportRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.reports.dto.ReportTestResult;
import com.IntegrityTechnologies.business_manager.modules.communication.reports.registry.ReportDefinition;
import com.IntegrityTechnologies.business_manager.modules.communication.reports.registry.ReportRegistry;
import com.IntegrityTechnologies.business_manager.modules.communication.reports.security.ReportAccess;
import com.IntegrityTechnologies.business_manager.modules.communication.reports.security.ReportLimitRegistry;
import com.IntegrityTechnologies.business_manager.modules.communication.reports.security.ReportLimits;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import net.sf.jasperreports.engine.*;
import net.sf.jasperreports.engine.export.JRCsvExporter;
import net.sf.jasperreports.engine.export.ooxml.JRXlsxExporter;
import net.sf.jasperreports.export.SimpleExporterInput;
import net.sf.jasperreports.export.SimpleOutputStreamExporterOutput;
import net.sf.jasperreports.export.SimpleWriterExporterOutput;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Service;
import org.springframework.beans.factory.annotation.Value;

import java.time.LocalDate;
import java.time.LocalDateTime;


import javax.sql.DataSource;
import java.io.InputStream;
import java.io.OutputStream;
import java.sql.Connection;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
@RequiredArgsConstructor
public class JasperReportingService implements ReportingService {

    private final DataSource dataSource;

    @Value("${app.company.name:Company}")
    private String companyName;

    @Value("${app.company.logo-path:}")
    private String logoPath;

    @Override
    public void generate(ReportRequest request, OutputStream out) throws Exception {

        // 1. Resolve report definition
        ReportDefinition def =
                ReportRegistry.get(request.getReportName());

        if (def == null) {
            throw new IllegalArgumentException(
                    "Unknown report: " + request.getReportName()
            );
        }

        // 2. Enforce access control
        ReportAccess access =
                ReportAccess.fromReportName(request.getReportName());

        if (!access.canAccess(SecurityUtils.currentRole())) {
            throw new SecurityException(
                    "Access denied for report: " + request.getReportName()
            );
        }

        // 3. Resolve parameters
        Map<String, Object> params = new HashMap<>();

        if (request.getParameters() != null) {
            params.putAll(request.getParameters());
        }

        // ===== System parameters =====
        params.put("GENERATED_BY", SecurityUtils.currentUsername());
        params.put("GENERATED_AT", LocalDateTime.now());
        params.put("REPORT_NAME", request.getReportName());
        params.put("COMPANY_NAME", companyName);

        if (logoPath != null && !logoPath.isBlank()) {
            params.put(
                    "LOGO_PATH",
                    new ClassPathResource(logoPath).getInputStream()
            );
        }

        // 4. Validate required parameters
        for (String required : def.requiredParams()) {
            if (!params.containsKey(required)) {
                throw new IllegalArgumentException(
                        "Missing required parameter: " + required
                );
            }
        }

        enforceDateLimits(request.getReportName(), params);

        // 5. Generate report
        try (
                InputStream jrxml =
                        new ClassPathResource(def.jrxmlPath()).getInputStream();
                Connection conn = dataSource.getConnection()
        ) {

            JasperReport report =
                    JasperCompileManager.compileReport(jrxml);

            JasperPrint jp =
                    JasperFillManager.fillReport(report, params, conn);

            ReportFormat format =
                    request.getFormat() != null
                            ? request.getFormat()
                            : ReportFormat.PDF;

            switch (format) {

                case XLSX -> {
                    JRXlsxExporter exporter = new JRXlsxExporter();
                    exporter.setExporterInput(new SimpleExporterInput(jp));
                    exporter.setExporterOutput(
                            new SimpleOutputStreamExporterOutput(out)
                    );
                    exporter.exportReport();
                }

                case CSV -> {
                    JRCsvExporter exporter = new JRCsvExporter();
                    exporter.setExporterInput(new SimpleExporterInput(jp));
                    exporter.setExporterOutput(
                            new SimpleWriterExporterOutput(out)
                    );
                    exporter.exportReport();
                }

                default -> JasperExportManager.exportReportToPdfStream(jp, out);
            }
        }
    }

    @Override
    public List<ReportTestResult> testAllReports() {

        SecurityUtils.requireAdmin();

        List<ReportTestResult> results = new ArrayList<>();

        for (ReportDefinition def : ReportRegistry.REPORTS.values()) {

            try (
                    InputStream jrxml =
                            new ClassPathResource(def.jrxmlPath()).getInputStream();
                    Connection conn = dataSource.getConnection()
            ) {

                JasperReport report =
                        JasperCompileManager.compileReport(jrxml);

                // Dummy parameters (null-safe)
                Map<String, Object> params = new HashMap<>();
                for (String p : def.requiredParams()) {
                    params.put(p, null);
                }

                JasperFillManager.fillReport(report, params, conn);

                results.add(
                        new ReportTestResult(def.key(), true, "OK")
                );

            } catch (Exception e) {
                results.add(
                        new ReportTestResult(
                                def.key(),
                                false,
                                e.getClass().getSimpleName() + ": " + e.getMessage()
                        )
                );
            }
        }

        return results;
    }

    private void enforceDateLimits(String reportName, Map<String, Object> params) {

        ReportLimits limits = ReportLimitRegistry.get(reportName);
        if (limits == null) return;

        if (params.containsKey("FROM_DATE") && params.containsKey("TO_DATE")) {

            LocalDate from = LocalDate.parse(params.get("FROM_DATE").toString());
            LocalDate to = LocalDate.parse(params.get("TO_DATE").toString());

            long diff = limits.unit().between(from, to);

            if (diff > limits.maxRange()) {
                throw new IllegalArgumentException(
                        "Date range too large for report " + reportName +
                                ". Max allowed: " + limits.maxRange() + " " + limits.unit()
                );
            }
        }
    }

}