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
import net.sf.jasperreports.engine.data.JRBeanCollectionDataSource;
import net.sf.jasperreports.engine.export.JRCsvExporter;
import net.sf.jasperreports.engine.export.ooxml.JRXlsxExporter;
import net.sf.jasperreports.export.SimpleExporterInput;
import net.sf.jasperreports.export.SimpleOutputStreamExporterOutput;
import net.sf.jasperreports.export.SimpleWriterExporterOutput;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Service;

import javax.sql.DataSource;
import java.io.InputStream;
import java.io.OutputStream;
import java.sql.Connection;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;

@Service
@RequiredArgsConstructor
public class JasperReportingService implements ReportingService {

    private final DataSource dataSource;
    private final FinancialReportDataProvider financialReportDataProvider;

    @Value("${app.company.name:Company}")
    private String companyName;

    @Value("${app.company.logo-path:}")
    private String logoPath;

    @Override
    public void generate(ReportRequest request, OutputStream out) throws Exception {

        if (request.getReportName() == null) {
            throw new IllegalArgumentException("Report name is required");
        }

        String baseReportKey = request.getReportName();

        Map<String, Object> params =
                request.getParameters() != null
                        ? new HashMap<>(request.getParameters())
                        : new HashMap<>();

        boolean branchProvided =
                params.containsKey("BRANCH_ID")
                        && params.get("BRANCH_ID") != null;

        ReportDefinition baseDef = ReportRegistry.get(baseReportKey);

        if (baseDef == null) {
            throw new IllegalArgumentException(
                    "Unknown report: " + baseReportKey
            );
        }

        // 🔥 Determine correct JRXML path dynamically
        String jrxmlPath = baseDef.jrxmlPath();

        if (!branchProvided) {
            String multiPath = jrxmlPath.replace(".jrxml", "_multi_branch.jrxml");
            ClassPathResource multiResource =
                    new ClassPathResource(multiPath);

            if (multiResource.exists()) {
                jrxmlPath = multiPath;
            }
        }

        // 🔐 Access check (always base report)
        ReportAccess access =
                ReportAccess.fromReportName(baseReportKey);

        if (!access.canAccess(SecurityUtils.currentRole())) {
            throw new SecurityException(
                    "Access denied for report: " + baseReportKey
            );
        }

        params.put("GENERATED_BY", SecurityUtils.currentUsername());
        params.put("GENERATED_AT", new java.util.Date());
        params.put("COMPANY_NAME", companyName);

        enforceDateLimits(baseReportKey, params);

        // 🔥 Smart parameter validation
        for (String required : baseDef.requiredParams()) {

            // Skip BRANCH_ID if consolidated
            if (!branchProvided && required.equals("BRANCH_ID")) {
                continue;
            }

            if (!params.containsKey(required)) {
                throw new IllegalArgumentException(
                        "Missing required parameter: " + required
                );
            }
        }

        try (InputStream jrxml =
                     new ClassPathResource(jrxmlPath).getInputStream()) {

            JasperReport report =
                    JasperCompileManager.compileReport(jrxml);

            JasperPrint jp;

            if (financialReportDataProvider.supports(baseReportKey)) {

                JRBeanCollectionDataSource ds =
                        financialReportDataProvider.provide(
                                baseReportKey,
                                params
                        );

                jp = JasperFillManager.fillReport(report, params, ds);

            } else {

                try (Connection conn = dataSource.getConnection()) {
                    jp = JasperFillManager.fillReport(report, params, conn);
                }
            }

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