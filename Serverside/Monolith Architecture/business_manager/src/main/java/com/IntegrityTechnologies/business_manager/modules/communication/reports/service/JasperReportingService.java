package com.IntegrityTechnologies.business_manager.modules.communication.reports.service;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.repository.SaleRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.SaleLineItem;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.StockTransactionRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
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
    private final ProductVariantRepository productVariantRepository;
    private final StockTransactionRepository stockTransactionRepository;

    /* ========================================================
       SALES SUMMARY PDF (variant-aware)
       ======================================================== */
    @Override
    public void generateSalesSummaryPdf(LocalDate from, LocalDate to, OutputStream out) throws Exception {

        List<Map<String, Object>> rows = new ArrayList<>();

        List<Sale> sales = saleRepository.findAll();

        // aggregate by variantId
        Map<UUID, Long> qtyMap = new HashMap<>();
        Map<UUID, BigDecimal> salesMap = new HashMap<>();

        for (Sale s : sales) {
            if (s.getCreatedAt() == null) continue;
            LocalDate d = s.getCreatedAt().toLocalDate();
            if (d.isBefore(from) || d.isAfter(to)) continue;

            for (SaleLineItem li : s.getLineItems()) {
                UUID variantId = li.getProductVariantId();
                qtyMap.put(variantId, qtyMap.getOrDefault(variantId, 0L) + li.getQuantity());
                salesMap.put(variantId, salesMap.getOrDefault(variantId, BigDecimal.ZERO).add(li.getLineTotal()));
            }
        }

        for (UUID variantId : qtyMap.keySet()) {
            ProductVariant variant = productVariantRepository.findById(variantId).orElse(null);
            String productName = "Unknown Product";
            String classification = "";

            if (variant != null) {
                Product p = variant.getProduct();
                if (p != null) productName = p.getName();
                classification = variant.getClassification() != null ? variant.getClassification() : "";
            }

            Map<String, Object> r = new HashMap<>();
            r.put("productName", productName);
            r.put("variantClassification", classification);
            r.put("quantitySold", qtyMap.get(variantId));
            r.put("totalSales", salesMap.get(variantId));
            rows.add(r);
        }

        InputStream jrxml = new ClassPathResource("reports/sales_summary.jrxml").getInputStream();
        JasperReport jr = JasperCompileManager.compileReport(jrxml);

        JRBeanCollectionDataSource ds = new JRBeanCollectionDataSource(rows);

        Map<String, Object> params = new HashMap<>();
        params.put("REPORT_TITLE", "Sales Summary (Variant-Aware)");
        params.put("FROM_DATE", from.toString());
        params.put("TO_DATE", to.toString());

        JasperPrint jp = JasperFillManager.fillReport(jr, params, ds);
        JasperExportManager.exportReportToPdfStream(jp, out);
    }


    /* ========================================================
       SALE RECEIPT PDF (variant-aware)
       ======================================================== */
    @Override
    public void generateSaleReceiptPdf(UUID saleId, OutputStream out) throws Exception {
        Sale sale = saleRepository.findById(saleId)
                .orElseThrow(() -> new IllegalArgumentException("Sale not found: " + saleId));

        List<Map<String, Object>> lines = sale.getLineItems().stream()
                .map(li -> {
                    Map<String, Object> m = new HashMap<>();

                    ProductVariant variant =
                            productVariantRepository.findById(li.getProductVariantId()).orElse(null);

                    String classification = variant != null ? variant.getClassification() : "";
                    String productName = li.getProductName();

                    m.put("productName", productName);
                    m.put("variantClassification", classification);
                    m.put("qty", li.getQuantity());
                    m.put("unitPrice", li.getUnitPrice());
                    m.put("lineTotal", li.getLineTotal());
                    return m;
                })
                .collect(Collectors.toList());

        InputStream jrxml = new ClassPathResource("reports/receipt.jrxml").getInputStream();
        JasperReport jr = JasperCompileManager.compileReport(jrxml);
        JRBeanCollectionDataSource ds = new JRBeanCollectionDataSource(lines);

        Map<String, Object> params = new HashMap<>();
        params.put("SALE_ID", sale.getId().toString());
        params.put("DATE", sale.getCreatedAt() != null ? sale.getCreatedAt().toString() : "");
        params.put("CASHIER", sale.getCreatedBy());
        params.put("CUSTOMER", "");

        JasperPrint jp = JasperFillManager.fillReport(jr, params, ds);
        JasperExportManager.exportReportToPdfStream(jp, out);
    }


    /* ========================================================
       PURCHASE ORDER PDF
       ======================================================== */
    @Override
    public void generatePurchaseOrderPdf(UUID purchaseOrderId, OutputStream out) throws Exception {

        InputStream jrxml = new ClassPathResource("reports/purchase_order.jrxml").getInputStream();
        JasperReport jr = JasperCompileManager.compileReport(jrxml);

        JRBeanCollectionDataSource ds = new JRBeanCollectionDataSource(Collections.emptyList());

        Map<String, Object> params = new HashMap<>();
        params.put("PO_NUMBER", purchaseOrderId.toString());
        params.put("SUPPLIER_NAME", "Supplier");
        params.put("DATE", LocalDate.now().toString());

        JasperPrint jp = JasperFillManager.fillReport(jr, params, ds);
        JasperExportManager.exportReportToPdfStream(jp, out);
    }
}