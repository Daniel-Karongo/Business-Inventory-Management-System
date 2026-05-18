package com.IntegrityTechnologies.business_manager.modules.procurement.matching.service;

import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.dto.*;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.service.PurchaseInvoicePostingService;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.service.PurchaseInvoiceService;
import com.IntegrityTechnologies.business_manager.modules.procurement.matching.dto.MatchInvoiceToReceiptsRequest;
import com.IntegrityTechnologies.business_manager.modules.procurement.matching.dto.ReceiveAndInvoiceRequest;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.ReceiveStockRequest;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.SupplierUnit;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.*;

@Service
@RequiredArgsConstructor
public class ReceiveAndInvoiceService {

    private final InventoryService inventoryService;
    private final PurchaseInvoiceService invoiceService;
    private final PurchaseInvoicePostingService postingService;
    private final GoodsReceiptMatchingService matchingService;

    @Transactional
    @SuppressWarnings("unchecked")
    public PurchaseInvoiceResponse execute(
            ReceiveAndInvoiceRequest request
    ) {

        ReceiveStockRequest stock =
                request.getStockReceipt();

        String orchestrationKey =
                stock.getReference();

        if (
                orchestrationKey == null ||
                        orchestrationKey.isBlank()
        ) {
            throw new IllegalArgumentException(
                    "Reference is required for orchestration safety"
            );
        }

        /*
         * STEP 1
         * RECEIVE STOCK
         */
        ApiResponse receiptResult =
                inventoryService.receiveStock(stock);

        Map<String, Object> response =
                (Map<String, Object>)
                        receiptResult.getData();

        List<UUID> goodsReceiptIds =
                (List<UUID>)
                        response.get("goodsReceiptIds");

        /*
         * STEP 2
         * CREATE INVOICE
         */
        CreatePurchaseInvoiceRequest invoice =
                new CreatePurchaseInvoiceRequest();

        invoice.setBranchId(
                stock.getBranchId()
        );

        invoice.setInvoiceDate(
                LocalDate.now()
        );

        invoice.setDueDate(
                LocalDate.now().plusDays(30)
        );

        invoice.setSupplierId(
                stock.getSuppliers()
                        .get(0)
                        .getSupplierId()
        );

        invoice.setSupplierInvoiceNumber(
                "AUTO-GRN-" +
                        orchestrationKey
        );

        invoice.setNotes(
                "Auto-generated from stock receipt"
        );

        List<CreatePurchaseInvoiceLineRequest>
                lines =
                new ArrayList<>();

        for (
                SupplierUnit supplier :
                stock.getSuppliers()
        ) {

            CreatePurchaseInvoiceLineRequest line =
                    new CreatePurchaseInvoiceLineRequest();

            line.setProductId(
                    stock.getProductId()
            );

            line.setProductVariantId(
                    stock.getProductVariantId()
            );

            line.setQuantity(
                    supplier.getUnitsSupplied()
            );

            line.setUnitCost(
                    supplier.getUnitCost()
            );

            line.setVatAmount(
                    BigDecimal.ZERO
            );

            line.setDiscountAmount(
                    BigDecimal.ZERO
            );

            lines.add(line);
        }

        invoice.setLines(lines);

        PurchaseInvoiceResponse created =
                invoiceService.create(invoice);

        /*
         * STEP 3
         * APPROVE
         */
        invoiceService.markApproved(
                stock.getBranchId(),
                created.getId()
        );

        /*
         * STEP 4
         * POST
         */
        postingService.post(
                stock.getBranchId(),
                created.getId()
        );

        /*
         * STEP 5
         * MATCH
         */
        MatchInvoiceToReceiptsRequest matching =
                new MatchInvoiceToReceiptsRequest();

        matching.setBranchId(
                stock.getBranchId()
        );

        matching.setPurchaseInvoiceId(
                created.getId()
        );

        matching.setGoodsReceiptIds(
                goodsReceiptIds
        );

        matchingService.match(matching);

        return invoiceService.get(
                stock.getBranchId(),
                created.getId()
        );
    }
}