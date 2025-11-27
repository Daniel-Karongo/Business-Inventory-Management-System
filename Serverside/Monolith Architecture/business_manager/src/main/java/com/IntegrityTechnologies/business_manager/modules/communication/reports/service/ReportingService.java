package com.IntegrityTechnologies.business_manager.modules.communication.reports.service;

import jakarta.servlet.http.HttpServletResponse;

import java.io.OutputStream;
import java.time.LocalDate;
import java.util.UUID;

public interface ReportingService {
    /**
     * Generate a sales summary report PDF (from->to) and write to the response output stream.
     */
    void generateSalesSummaryPdf(LocalDate from, LocalDate to, OutputStream out) throws Exception;

    /**
     * Generate a receipt PDF for a single sale.
     */
    void generateSaleReceiptPdf(UUID saleId, OutputStream out) throws Exception;

    /**
     * Generate purchase order PDF
     */
    void generatePurchaseOrderPdf(UUID purchaseOrderId, OutputStream out) throws Exception;
}