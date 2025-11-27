package com.IntegrityTechnologies.business_manager.modules.communication.reports.service;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.repository.SaleRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.SaleLineItem;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.StockTransactionRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.repository.ProductRepository;

import lombok.RequiredArgsConstructor;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Service;

import net.sf.jasperreports.engine.*;
import net.sf.jasperreports.engine.data.JRBeanCollectionDataSource;

import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class JasperReportingService implements ReportingService {

    private final SaleRepository saleRepository;
    private final ProductRepository productRepository;
    private final StockTransactionRepository stockTransactionRepository;

    @Override
    public void generateSalesSummaryPdf(LocalDate from, LocalDate to, OutputStream out) throws Exception {
        // For simplicity: build an aggregated list of products with qty and total sales between dates
        // In production, write a custom JPQL/SQL query that groups by product_id to avoid pulling many records
        List<Map<String, Object>> rows = new ArrayList<>();

        // naive approach: iterate sales in date range
        List<Sale> sales = saleRepository.findAll(); // you should implement findByCreatedAtBetween for efficiency
        Map<UUID, Long> qtyMap = new HashMap<>();
        Map<UUID, BigDecimal> salesMap = new HashMap<>();

        for (Sale s : sales) {
            if (s.getCreatedAt() == null) continue;
            boolean inRange = !s.getCreatedAt().toLocalDate().isBefore(from) && !s.getCreatedAt().toLocalDate().isAfter(to);
            if (!inRange) continue;
            for (SaleLineItem li : s.getLineItems()) {
                qtyMap.put(li.getProductId(), qtyMap.getOrDefault(li.getProductId(), 0L) + li.getQuantity());
                salesMap.put(li.getProductId(), salesMap.getOrDefault(li.getProductId(), BigDecimal.ZERO).add(li.getLineTotal()));
            }
        }

        for (UUID pid : qtyMap.keySet()) {
            String name = productRepository.findById(pid).map(p -> p.getName()).orElse("Unknown");
            Map<String, Object> r = new HashMap<>();
            r.put("productName", name);
            r.put("quantitySold", qtyMap.get(pid));
            r.put("totalSales", salesMap.get(pid));
            rows.add(r);
        }

        // Compile jrxml and fill
        InputStream jrxml = new ClassPathResource("reports/sales_summary.jrxml").getInputStream();
        JasperReport jr = JasperCompileManager.compileReport(jrxml);
        JRBeanCollectionDataSource ds = new JRBeanCollectionDataSource(rows);

        Map<String, Object> params = new HashMap<>();
        params.put("REPORT_TITLE", "Sales Summary");
        params.put("FROM_DATE", from.toString());
        params.put("TO_DATE", to.toString());

        JasperPrint jp = JasperFillManager.fillReport(jr, params, ds);
        JasperExportManager.exportReportToPdfStream(jp, out);
    }

    @Override
    public void generateSaleReceiptPdf(UUID saleId, OutputStream out) throws Exception {
        Sale sale = saleRepository.findById(saleId).orElseThrow(() -> new IllegalArgumentException("Sale not found: " + saleId));

        List<Map<String, Object>> lines = sale.getLineItems().stream().map(li -> {
            Map<String, Object> m = new HashMap<>();
            m.put("productName", li.getProductName());
            m.put("qty", li.getQuantity());
            m.put("unitPrice", li.getUnitPrice());
            m.put("lineTotal", li.getLineTotal());
            return m;
        }).collect(Collectors.toList());

        InputStream jrxml = new ClassPathResource("reports/receipt.jrxml").getInputStream();
        JasperReport jr = JasperCompileManager.compileReport(jrxml);
        JRBeanCollectionDataSource ds = new JRBeanCollectionDataSource(lines);

        Map<String, Object> params = new HashMap<>();
        params.put("SALE_ID", sale.getId().toString());
        params.put("DATE", sale.getCreatedAt() != null ? sale.getCreatedAt().toString() : "");
        params.put("CASHIER", sale.getCreatedBy());
        params.put("CUSTOMER", ""); // if sale has link to customer, set here

        JasperPrint jp = JasperFillManager.fillReport(jr, params, ds);
        JasperExportManager.exportReportToPdfStream(jp, out);
    }

    @Override
    public void generatePurchaseOrderPdf(UUID purchaseOrderId, OutputStream out) throws Exception {
        // Minimal implementation: load PO model, build lines, fill jrxml
        // You must implement PurchaseOrder model/repo; I'll leave sample behavior
        InputStream jrxml = new ClassPathResource("reports/purchase_order.jrxml").getInputStream();
        JasperReport jr = JasperCompileManager.compileReport(jrxml);
        // create empty data source for sample
        JRBeanCollectionDataSource ds = new JRBeanCollectionDataSource(Collections.emptyList());
        Map<String, Object> params = new HashMap<>();
        params.put("PO_NUMBER", purchaseOrderId.toString());
        params.put("SUPPLIER_NAME", "Supplier");
        params.put("DATE", LocalDate.now().toString());

        JasperPrint jp = JasperFillManager.fillReport(jr, params, ds);
        JasperExportManager.exportReportToPdfStream(jp, out);
    }
}