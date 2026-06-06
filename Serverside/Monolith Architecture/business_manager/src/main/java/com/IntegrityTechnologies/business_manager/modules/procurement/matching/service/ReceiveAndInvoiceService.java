package com.IntegrityTechnologies.business_manager.modules.procurement.matching.service;

import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.domain.PurchaseInvoice;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.dto.*;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.repository.PurchaseInvoiceRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.service.PurchaseInvoicePostingService;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.service.PurchaseInvoiceService;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.shared.mapper.PurchaseInvoiceMapper;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.base.config.TaxProperties;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto.VatBreakdown;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.service.VatCalculationService;
import com.IntegrityTechnologies.business_manager.modules.procurement.matching.dto.MatchInvoiceToReceiptsRequest;
import com.IntegrityTechnologies.business_manager.modules.procurement.matching.dto.ReceiveAndInvoiceRequest;
import com.IntegrityTechnologies.business_manager.modules.procurement.receipt.repository.GoodsReceiptRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.ReceiveStockRequest;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.SupplierUnit;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.time.LocalDate;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class ReceiveAndInvoiceService {

    private final InventoryService inventoryService;
    private final PurchaseInvoiceService invoiceService;
    private final PurchaseInvoicePostingService postingService;
    private final GoodsReceiptMatchingService matchingService;
    private final PurchaseInvoiceRepository invoiceRepository;
    private final PurchaseInvoiceMapper invoiceMapper;
    private final GoodsReceiptRepository goodsReceiptRepository;
    private final VatCalculationService vatCalculationService;
    private final TaxProperties taxProperties;

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
         * =====================================================
         * PARTITION SUPPLIERS
         * =====================================================
         */

        Map<UUID, List<SupplierUnit>> supplierPartitions =
                stock.getSuppliers()
                        .stream()
                        .collect(
                                Collectors.groupingBy(
                                        SupplierUnit::getSupplierId,
                                        LinkedHashMap::new,
                                        Collectors.toList()
                                )
                        );

        PurchaseInvoiceResponse lastInvoice = null;

        /*
         * =====================================================
         * PROCESS EACH SUPPLIER IN SAME TX
         * =====================================================
         */

        for (
                Map.Entry<UUID, List<SupplierUnit>> entry :
                supplierPartitions.entrySet()
        ) {

            UUID supplierId =
                    entry.getKey();

            String deterministicReceiptNumber =
                    deterministicReceiptNumber(
                            supplierId,
                            orchestrationKey
                    );

            boolean receiptAlreadyExists =
                    goodsReceiptRepository
                            .findByTenantIdAndBranchIdAndReceiptNumber(
                                    TenantContext.getTenantId(),
                                    stock.getBranchId(),
                                    deterministicReceiptNumber
                            )
                            .isPresent();

            if (receiptAlreadyExists) {
                continue;
            }

            List<SupplierUnit> supplierUnits =
                    entry.getValue();

            /*
             * ---------------------------------------------
             * SUPPLIER-SCOPED RECEIPT
             * ---------------------------------------------
             */

            ReceiveStockRequest supplierReceipt =
                    new ReceiveStockRequest();

            supplierReceipt.setBranchId(
                    stock.getBranchId()
            );

            supplierReceipt.setProductId(
                    stock.getProductId()
            );

            supplierReceipt.setProductVariantId(
                    stock.getProductVariantId()
            );

            supplierReceipt.setNote(
                    stock.getNote()
            );

            supplierReceipt.setReference(
                    orchestrationKey
                            + "::SUPPLIER::"
                            + supplierId
            );

            supplierReceipt.setSuppliers(
                    supplierUnits
            );

            /*
             * ---------------------------------------------
             * RECEIVE STOCK
             * ---------------------------------------------
             */

            ApiResponse receiptResult =
                    inventoryService.receiveStock(
                            supplierReceipt
                    );

            Map<String, Object> response =
                    (Map<String, Object>)
                            receiptResult.getData();

            List<UUID> goodsReceiptIds =
                    (List<UUID>)
                            response.get("goodsReceiptIds");

            /*
             * ---------------------------------------------
             * CREATE SUPPLIER INVOICE
             * ---------------------------------------------
             */

            CreatePurchaseInvoiceRequest invoice =
                    new CreatePurchaseInvoiceRequest();

            invoice.setBranchId(
                    stock.getBranchId()
            );

            LocalDate accountingDate =
                    request.getAccountingDate();

            invoice.setInvoiceDate(
                    accountingDate
            );

            invoice.setDueDate(
                    accountingDate.plusDays(30)
            );

            invoice.setSupplierId(
                    supplierId
            );

            invoice.setSupplierInvoiceNumber(
                    "AUTO-GRN-"
                            + orchestrationKey
                            + "-"
                            + supplierId
            );

            invoice.setNotes(
                    "Auto-generated from stock receipt"
            );

            List<CreatePurchaseInvoiceLineRequest>
                    lines =
                    new ArrayList<>();

            for (
                    SupplierUnit supplier :
                    supplierUnits
            ) {

                CreatePurchaseInvoiceLineRequest line =
                        new CreatePurchaseInvoiceLineRequest();

                line.setProductId(
                        stock.getProductId()
                );

                line.setProductVariantId(
                        stock.getProductVariantId()
                );

                boolean vatInclusive =
                        supplier.getVatInclusive() != null
                                ? supplier.getVatInclusive()
                                : taxProperties.isPricesVatInclusive();

                BigDecimal vatRate =
                        supplier.getVatRate() != null
                                ? supplier.getVatRate()
                                : taxProperties.getVatRate();

                BigDecimal grossAmount =
                        supplier.getUnitCost()
                                .multiply(
                                        BigDecimal.valueOf(
                                                supplier.getUnitsSupplied()
                                        )
                                );

                VatBreakdown persistedBreakdown =
                        vatCalculationService.calculate(
                                grossAmount,
                                vatInclusive,
                                vatRate
                        );

                line.setQuantity(
                        supplier.getUnitsSupplied()
                );

                line.setUnitCost(
                        persistedBreakdown.net()
                                .divide(
                                        BigDecimal.valueOf(
                                                supplier.getUnitsSupplied()
                                        ),
                                        6,
                                        RoundingMode.HALF_UP
                                )
                );

                line.setVatAmount(
                        persistedBreakdown.vat()
                );

                line.setLineSubtotal(
                        persistedBreakdown.net()
                );

                line.setLineTotal(
                        persistedBreakdown.gross()
                );

                line.setDiscountAmount(
                        BigDecimal.ZERO
                );

                line.setVatInclusive(vatInclusive);
                line.setVatRate(vatRate);

                lines.add(line);
            }

            invoice.setLines(lines);

            Optional<PurchaseInvoice> existingInvoice =
                    invoiceRepository
                            .findByTenantIdAndBranchIdAndSupplierInvoiceNumber(
                                    TenantContext.getTenantId(),
                                    stock.getBranchId(),
                                    invoice.getSupplierInvoiceNumber()
                            );

            if (existingInvoice.isPresent()) {

                lastInvoice =
                        invoiceMapper.toResponse(
                                existingInvoice.get()
                        );

                continue;
            }
            
            PurchaseInvoiceResponse created =
                    invoiceService.create(invoice);

            /*
             * ---------------------------------------------
             * APPROVE
             * ---------------------------------------------
             */

            invoiceService.markApproved(
                    stock.getBranchId(),
                    created.getId()
            );

            /*
             * ---------------------------------------------
             * POST
             * ---------------------------------------------
             */

            postingService.post(
                    stock.getBranchId(),
                    created.getId()
            );

            /*
             * ---------------------------------------------
             * MATCH
             * ---------------------------------------------
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

            matchingService.match(
                    matching
            );

            lastInvoice = created;
        }

        return lastInvoice;
    }

    private String deterministicReceiptNumber(
            UUID supplierId,
            String reference
    ) {
        return "GRN-" +
                UUID.nameUUIDFromBytes(
                        (
                                "GRN:" +
                                        supplierId +
                                        ":" +
                                        reference
                        ).getBytes(StandardCharsets.UTF_8)
                );
    }
}