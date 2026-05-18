package com.IntegrityTechnologies.business_manager.modules.finance.ap.shared.mapper;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.domain.PurchaseInvoice;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.domain.PurchaseInvoiceLine;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.dto.PurchaseInvoiceLineResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.dto.PurchaseInvoiceResponse;
import org.springframework.stereotype.Component;

import java.util.stream.Collectors;

@Component
public class PurchaseInvoiceMapper {

    public PurchaseInvoiceResponse toResponse(
            PurchaseInvoice invoice
    ) {

        return PurchaseInvoiceResponse
                .builder()
                .id(invoice.getId())
                .documentNumber(
                        invoice.getDocumentNumber()
                )
                .supplierId(
                        invoice.getSupplier().getId()
                )
                .supplierName(
                        invoice.getSupplier().getName()
                )
                .supplierInvoiceNumber(
                        invoice.getSupplierInvoiceNumber()
                )
                .invoiceDate(
                        invoice.getDocumentDate()
                )
                .dueDate(
                        invoice.getDueDate()
                )
                .subtotal(invoice.getSubtotal())
                .vatAmount(invoice.getVatAmount())
                .discountAmount(
                        invoice.getDiscountAmount()
                )
                .totalAmount(invoice.getTotalAmount())
                .allocatedAmount(
                        invoice.getAllocatedAmount()
                )
                .outstandingAmount(
                        invoice.getOutstandingAmount()
                )
                .status(invoice.getStatus())
                .postingStatus(
                        invoice.getPostingLifecycleStatus()
                )
                .overdue(invoice.isOverdue())
                .fullyAllocated(
                        invoice.isFullyAllocated()
                )
                .posted(invoice.isPosted())
                .postedAt(invoice.getPostedAt())
                .postedBy(invoice.getPostedBy())
                .notes(invoice.getNotes())
                .lines(
                        invoice.getLines()
                                .stream()
                                .map(this::toLineResponse)
                                .collect(Collectors.toList())
                )
                .build();
    }

    private PurchaseInvoiceLineResponse
    toLineResponse(
            PurchaseInvoiceLine line
    ) {

        return PurchaseInvoiceLineResponse
                .builder()
                .id(line.getId())
                .productId(
                        line.getProduct() != null
                                ? line.getProduct().getId()
                                : null
                )
                .productVariantId(
                        line.getProductVariant() != null
                                ? line.getProductVariant().getId()
                                : null
                )
                .productName(
                        line.getProductNameSnapshot()
                )
                .productSku(
                        line.getProductSkuSnapshot()
                )
                .variantName(
                        line.getVariantNameSnapshot()
                )
                .quantity(line.getQuantity())
                .unitCost(line.getUnitCost())
                .discountAmount(
                        line.getDiscountAmount()
                )
                .vatAmount(line.getVatAmount())
                .lineSubtotal(
                        line.getLineSubtotal()
                )
                .lineTotal(line.getLineTotal())
                .build();
    }
}