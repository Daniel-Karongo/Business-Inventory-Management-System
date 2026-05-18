package com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.enums.PurchaseInvoicePostingStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.enums.PurchaseInvoiceStatus;
import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
@Builder
public class PurchaseInvoiceResponse {

    private UUID id;

    private String documentNumber;

    private UUID supplierId;

    private String supplierName;

    private String supplierInvoiceNumber;

    private LocalDate invoiceDate;

    private LocalDate dueDate;

    private BigDecimal subtotal;

    private BigDecimal vatAmount;

    private BigDecimal discountAmount;

    private BigDecimal totalAmount;

    private BigDecimal allocatedAmount;

    private BigDecimal outstandingAmount;

    private PurchaseInvoiceStatus status;

    private PurchaseInvoicePostingStatus postingStatus;

    private boolean overdue;

    private boolean fullyAllocated;

    private boolean posted;

    private LocalDateTime postedAt;

    private String postedBy;

    private String notes;

    private List<PurchaseInvoiceLineResponse> lines;
}