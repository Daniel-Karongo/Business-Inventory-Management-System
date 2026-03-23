package com.IntegrityTechnologies.business_manager.modules.finance.sales.service;

import com.IntegrityTechnologies.business_manager.config.bulk.*;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkError;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkOptions;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkRequest;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkResult;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.AccountingSystemStateService;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.service.PaymentService;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.dto.*;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.SaleLineItem;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.repository.SaleRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.service.TaxSystemStateService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.model.ProductVariantPackaging;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.service.ProductVariantPackagingService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingAdjustment;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingContext;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingPolicy;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingResult;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.service.PricingEngineService;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
import com.IntegrityTechnologies.business_manager.security.util.SecurityUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.interceptor.TransactionAspectSupport;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.*;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional
public class SaleBulkService {

    private final SaleRepository saleRepository;
    private final ProductVariantRepository productVariantRepository;
    private final InventoryService inventoryService;
    private final PaymentService paymentService;
    private final BranchRepository branchRepository;
    private final ReceiptNumberService receiptNumberService;
    private final ProductVariantPackagingService packagingService;
    private final PricingEngineService pricingEngine;
    private final ObjectMapper objectMapper;
    private final TaxSystemStateService taxSystemStateService;

    /* ============================================================
       ENTRY POINT
       ============================================================ */
    public BulkResult<SaleBulkPreviewRow> importSales(
            BulkRequest<SaleBulkRow> request,
            SaleImportMode mode
    ) {

        BulkOptions options =
                request.getOptions() != null
                        ? request.getOptions()
                        : new BulkOptions();

        BulkResult<SaleBulkPreviewRow> result = new BulkResult<>();
        result.setTotal(request.getItems().size());

        ValidationOutcome outcome =
                validateAndPlan(request.getItems(), result);

        // 🔢 ALWAYS sort errors before returning
        if (!result.getErrors().isEmpty()) {
            result.getErrors().sort(
                    Comparator.comparingInt(BulkError::getRow)
            );
            return result;
        }


        // ✅ EXECUTION ONLY WHEN SAFE
        if (!options.isDryRun()) {
            execute(outcome.receipts(), mode);
        }

        // 🔒 HARD GUARANTEE
        if (options.isDryRun()) {
            TransactionAspectSupport
                    .currentTransactionStatus()
                    .setRollbackOnly();
        }

        // 🔢 ensure errors are ordered by row number
        result.getErrors().sort(
                Comparator.comparingInt(BulkError::getRow)
        );

        return result;
    }

    /* ============================================================
       PHASE 1 — VALIDATION & PLANNING
       ============================================================ */
    private ValidationOutcome validateAndPlan(
            List<SaleBulkRow> rows,
            BulkResult<SaleBulkPreviewRow> result
    ) {

        Map<String, List<SaleBulkRow>> grouped =
                rows.stream()
                        .collect(Collectors.groupingBy(r -> {
                            if (r.getReceiptNo() != null && !r.getReceiptNo().isBlank()) {
                                return r.getReceiptNo().trim();
                            }
                            return UUID.randomUUID().toString();
                        }));

        // ---------- row-level validation ----------
        for (int i = 0; i < rows.size(); i++) {
            SaleBulkRow row = rows.get(i);
            int rowNum = i + 1;

            try {
                validateRow(row);

                ProductVariant variant = resolveVariant(row);
                Branch branch = resolveBranch(row);

                ProductVariantPackaging packaging =
                        resolvePackaging(row, variant);

                long baseUnits =
                        row.getQuantity() * packaging.getUnitsPerPackaging();

                try {
                    inventoryService.previewAllocation(
                            variant.getId(),
                            branch.getId(),
                            baseUnits,
                            null // no manual selection in bulk (for now)
                    );
                } catch (Exception ex) {
                    throw new IllegalArgumentException(
                            "Insufficient stock (strict allocation failed): " + ex.getMessage()
                    );
                }

            } catch (Exception ex) {
                result.addError(rowNum, ex.getMessage());
            }
        }

        if (!result.getErrors().isEmpty()) {
            return new ValidationOutcome(List.of());
        }

        // ---------- receipt-level validation ----------
        List<PlannedReceipt> plans = new ArrayList<>();

        for (var entry : grouped.entrySet()) {
            try {
                plans.add(planReceipt(entry.getValue()));
            } catch (Exception ex) {
                for (SaleBulkRow r : entry.getValue()) {
                    int idx = rows.indexOf(r) + 1;
                    result.addError(idx, ex.getMessage());
                }
            }
        }

        if (!result.getErrors().isEmpty()) {
            return new ValidationOutcome(List.of());
        }

        // ---------- success marking ----------
        for (SaleBulkRow row : rows) {
            result.addSuccess(toPreview(row));
        }

        return new ValidationOutcome(plans);
    }

    /* ============================================================
       RECEIPT PLANNING
       ============================================================ */
    private PlannedReceipt planReceipt(
            List<SaleBulkRow> rows
    ) {

        String receiptNo =
                rows.get(0).getReceiptNo() != null &&
                        !rows.get(0).getReceiptNo().isBlank()
                        ? rows.get(0).getReceiptNo().trim()
                        : receiptNumberService.nextSaleReceipt();

        validateReceiptNo(receiptNo);

        Branch branch = resolveBranch(rows.get(0));
        UUID customerId = rows.get(0).getCustomerId();
        UUID customerGroupId = rows.get(0).getCustomerGroupId();

        List<PlannedLineItem> items = new ArrayList<>();

        for (SaleBulkRow row : rows) {

            if (!Objects.equals(customerId, row.getCustomerId())) {
                throw new IllegalArgumentException("Mixed customerId in same receipt");
            }

            ProductVariant variant = resolveVariant(row);

            ProductVariantPackaging packaging =
                    resolvePackaging(row, variant);

            long baseUnits =
                    row.getQuantity() * packaging.getUnitsPerPackaging();

            PricingResult pricing;

            if (row.getUnitPrice() != null) {

                // 👤 MANUAL OVERRIDE
                pricing = new PricingResult();
                pricing.setBasePrice(row.getUnitPrice());
                pricing.setFinalPrice(row.getUnitPrice());
                pricing.setResolvedPriceId(null);

                pricing.getAdjustments().add(
                        new PricingAdjustment(
                                "MANUAL_OVERRIDE",
                                "BULK_INPUT",
                                BigDecimal.ZERO,
                                "Manual price override (bulk)"
                        )
                );

                if (variant.getMinimumSellingPrice() != null &&
                        row.getUnitPrice().compareTo(variant.getMinimumSellingPrice()) < 0) {

                    pricing.getAdjustments().add(
                            new PricingAdjustment(
                                    "BELOW_MIN_PRICE",
                                    "SYSTEM_WARNING",
                                    variant.getMinimumSellingPrice().subtract(row.getUnitPrice()),
                                    "Price below minimum selling price"
                            )
                    );
                }

            } else {

                pricing = pricingEngine.resolve(
                        PricingContext.builder()
                                .tenantId(TenantContext.getTenantId())
                                .branchId(branch.getId())
                                .productVariantId(variant.getId())
                                .packagingId(packaging.getId())
                                .quantity(baseUnits)
                                .customerId(customerId)           // ✅ ADD
                                .customerGroupId(customerGroupId) // ✅ ADD
                                .pricingTime(LocalDateTime.now())
                                .policy(
                                        PricingPolicy.builder()
                                                .enforceMinimumPrice(false) // bulk import allows override
                                                .requireOverrideReason(false) // optional depending on business
                                                .build()
                                )
                                .build()
                );
            }
            BigDecimal unitPrice = pricing.getFinalPrice();

            BigDecimal gross = unitPrice.multiply(BigDecimal.valueOf(row.getQuantity()));

            var taxState = taxSystemStateService.getOrCreate(branch.getId());

            boolean vatEnabled = taxState.isVatEnabled();
            BigDecimal vatRate = vatEnabled ? taxState.getVatRate() : BigDecimal.ZERO;
            boolean pricesVatInclusive = taxState.isPricesVatInclusive();

            BigDecimal net;
            BigDecimal vat;

            if (vatEnabled) {

                if (pricesVatInclusive) {
                    net = gross.divide(
                            BigDecimal.ONE.add(vatRate),
                            6,
                            RoundingMode.HALF_UP
                    );
                    vat = gross.subtract(net);
                } else {
                    net = gross;
                    vat = net.multiply(vatRate);
                    gross = net.add(vat);
                }

            } else {
                net = gross;
                vat = BigDecimal.ZERO;
            }

            String pricingJson;
            try {
                pricingJson = objectMapper.writeValueAsString(pricing);
            } catch (Exception e) {
                pricingJson = "{\"error\":\"pricing_serialization_failed\"}";
            }
            items.add(
                    new PlannedLineItem(
                            variant,
                            branch,
                            packaging,
                            row.getQuantity(),
                            baseUnits,
                            unitPrice,
                            net,
                            vat,
                            vatRate,
                            gross,
                            pricingJson
                    )
            );
        }

        BigDecimal total =
                items.stream()
                        .map(PlannedLineItem::lineTotal)
                        .reduce(BigDecimal.ZERO, BigDecimal::add);

        String paymentsRaw =
                rows.stream()
                        .map(SaleBulkRow::getPayments)
                        .filter(p -> p != null && !p.isBlank())
                        .findFirst()
                        .orElse(null);

        validatePaymentsAggregate(paymentsRaw, total);

        // provider reference validation (dry-run safe)
        if (paymentsRaw != null) {
            for (String entry : paymentsRaw.split(";")) {
                String[] parts = entry.split("\\|");
                if (parts.length >= 3) {
                    PaymentRequest pr = new PaymentRequest();
                    pr.setProviderReference(parts[2].trim());
                    paymentService.validatePaymentRequest(pr);
                }
            }
        }

        return new PlannedReceipt(
                receiptNo,
                resolveSaleDate(rows.get(0).getSaleDate()),
                items,
                paymentsRaw,
                total
        );
    }

    /* ============================================================
       PHASE 2 — EXECUTION
       ============================================================ */
    private void execute(
            List<PlannedReceipt> receipts,
            SaleImportMode mode
    ) {

        String currentUser = SecurityUtils.currentUsername();

        for (PlannedReceipt plan : receipts) {

            BigDecimal totalTax = plan.items().stream()
                    .map(PlannedLineItem::vatAmount)
                    .reduce(BigDecimal.ZERO, BigDecimal::add);

            Sale sale = Sale.builder()
                    .id(UUID.randomUUID())
                    .tenantId(TenantContext.getTenantId())
                    .branchId(plan.items().get(0).branch().getId())
                    .createdAt(plan.saleDate())
                    .createdBy(currentUser)
                    .receiptNo(plan.receiptNo())
                    .totalAmount(plan.total())
                    .totalDiscount(BigDecimal.ZERO)
                    .totalTax(totalTax) // ✅ FIXED
                    .status(Sale.SaleStatus.CREATED)
                    .lineItems(new ArrayList<>())
                    .build();

            for (PlannedLineItem li : plan.items()) {
                sale.getLineItems().add(
                    SaleLineItem.builder()
                        .productVariantId(li.variant().getId())
                        .productName(
                                li.variant().getProduct().getName() +
                                        " (" + li.variant().getClassification() + ")"
                        )
                        .branchId(li.branch().getId())
                        // 🔥 PACKAGING
                        .packagingId(li.packaging().getId())
                        .unitsPerPackaging(li.packaging().getUnitsPerPackaging())
                        // 🔥 QUANTITIES
                        .quantity(li.quantity())
                        .baseUnits(li.baseUnits())
                        // 🔥 PRICE
                        .unitPrice(li.unitPrice())
                        .lineTotal(li.lineTotal())
                        .netAmount(li.netAmount())
                        .vatRate(li.vatRate())
                        .vatAmount(li.vatAmount())
                        // 🔥 AUDIT
                        .pricingBreakdownJson(li.pricingJson())
                        .build()
                );
            }

            saleRepository.save(sale);

            if (mode == SaleImportMode.OPERATIONAL) {
                for (PlannedLineItem li : plan.items()) {
                    inventoryService.reserveStockVariant(
                            li.variant().getId(),
                            li.branch().getId(),
                            li.baseUnits(),
                            "SALE:" + sale.getId(),
                            null
                    );
                }
            }

            if (mode == SaleImportMode.OPERATIONAL && plan.paymentsRaw() != null) {
                applyPayments(sale, plan.paymentsRaw());
            }

            if (mode == SaleImportMode.HISTORICAL) {
                sale.setStatus(Sale.SaleStatus.COMPLETED);
                saleRepository.save(sale);
            }
        }
    }

    /* ============================================================
       PAYMENTS
       ============================================================ */
    private void applyPayments(Sale sale, String raw) {

        for (String entry : raw.split(";")) {
            String[] parts = entry.split("\\|");
            if (parts.length < 2) continue;

            BigDecimal amount = new BigDecimal(parts[1].trim());
            if (amount.compareTo(BigDecimal.ZERO) <= 0) continue;

            PaymentRequest pr = new PaymentRequest();
            pr.setSaleId(sale.getId());
            pr.setMethod(parts[0].trim());
            pr.setAmount(amount);
            pr.setProviderReference(
                    parts.length >= 3 ? parts[2].trim() : null
            );
            pr.setNote("Bulk import payment");

            paymentService.processPayment(pr);
        }
    }

    /* ============================================================
       VALIDATION HELPERS
       ============================================================ */
    private ProductVariantPackaging resolvePackaging(
            SaleBulkRow row,
            ProductVariant variant
    ) {
        if (row.getPackagingId() != null) {
            return packagingService.getPackagings(variant.getId())
                    .stream()
                    .filter(p -> p.getId().equals(row.getPackagingId()))
                    .findFirst()
                    .orElseThrow(() ->
                            new IllegalArgumentException("Invalid packaging"));
        }

        return packagingService.getBasePackaging(variant.getId());
    }

    private void validateRow(SaleBulkRow row) {

        if (row.getQuantity() == null || row.getQuantity() <= 0) {
            throw new IllegalArgumentException("quantity must be > 0");
        }

        if (row.getUnitPrice() != null &&
                row.getUnitPrice().compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("unitPrice must be >= 0");
        }
    }

    private ProductVariant resolveVariant(SaleBulkRow row) {

        String classification =
                Optional.ofNullable(row.getVariant())
                        .map(v -> v.trim().toUpperCase())
                        .orElse("STANDARD");

        UUID tenantId = TenantContext.getTenantId();
        UUID branchId = resolveBranch(row).getId();

        if (row.getSku() != null && !row.getSku().isBlank()) {

            ProductVariant v =
                    productVariantRepository
                            .findByTenantIdAndBranchIdAndSkuAndDeletedFalse(
                                    tenantId,
                                    branchId,
                                    row.getSku().trim()
                            )
                            .orElseThrow(() ->
                                    new IllegalArgumentException("SKU not found: " + row.getSku()));

            if (!classification.equalsIgnoreCase(v.getClassification())) {
                throw new IllegalArgumentException(
                        "Variant mismatch for SKU " + row.getSku());
            }

            return v;
        }

        if (row.getProductName() != null && !row.getProductName().isBlank()) {

            List<ProductVariant> variants =
                    productVariantRepository.findByProductNameAndClassificationSafe(
                            row.getProductName().trim(),
                            classification,
                            tenantId
                    );

            if (variants.isEmpty()) {
                throw new IllegalArgumentException(
                        "Product not found: " + row.getProductName());
            }

            if (variants.size() > 1) {
                throw new IllegalArgumentException(
                        "Ambiguous product: " + row.getProductName());
            }

            return variants.get(0);
        }

        throw new IllegalArgumentException("Either sku or productName is required");
    }

    private Branch resolveBranch(SaleBulkRow row) {

        String code =
                Optional.ofNullable(row.getBranchCode())
                        .filter(s -> !s.isBlank())
                        .orElse("MAIN");

        return branchRepository.findByTenantIdAndBranchCodeIgnoreCase(
                        TenantContext.getTenantId(),
                        code
                )
                .orElseThrow(() ->
                        new IllegalArgumentException("Unknown branchCode: " + code));
    }

    private LocalDateTime resolveSaleDate(String raw) {

        if (raw == null || raw.isBlank()) {
            return LocalDateTime.now();
        }

        try {
            return OffsetDateTime.parse(raw).toLocalDateTime();
        } catch (Exception ignored) {}

        try {
            return LocalDateTime.parse(raw);
        } catch (Exception ignored) {}

        try {
            return LocalDate.parse(raw).atStartOfDay();
        } catch (Exception ignored) {}

        throw new IllegalArgumentException("Invalid saleDate: " + raw);
    }

    private void validatePaymentsAggregate(
            String paymentsRaw,
            BigDecimal saleTotal
    ) {

        if (paymentsRaw == null || paymentsRaw.isBlank()) return;

        BigDecimal totalPaid = BigDecimal.ZERO;

        for (String entry : paymentsRaw.split(";")) {
            String[] parts = entry.split("\\|");
            if (parts.length < 2) continue;

            BigDecimal amount;
            try {
                amount = new BigDecimal(parts[1].trim());
            } catch (Exception ex) {
                throw new IllegalArgumentException(
                        "Invalid payment amount: " + parts[1]
                );
            }

            if (amount.compareTo(BigDecimal.ZERO) <= 0) continue;

            totalPaid = totalPaid.add(amount);
        }

        if (totalPaid.compareTo(saleTotal) > 0) {
            BigDecimal excess = totalPaid.subtract(saleTotal);

            throw new IllegalArgumentException(
                    "Payment exceeds remaining balance by " + excess
            );
        }
    }

    private void validateReceiptNo(String receiptNo) {

        if (receiptNo == null || receiptNo.isBlank()) return;

        if (!receiptNo.matches("[A-Za-z0-9\\-_/]+")) {
            throw new IllegalArgumentException(
                    "Invalid receiptNo format: " + receiptNo);
        }

        if (saleRepository.existsByReceiptNo(receiptNo)) {
            throw new IllegalArgumentException(
                    "Receipt number already exists: " + receiptNo);
        }
    }

    private SaleBulkPreviewRow toPreview(SaleBulkRow r) {

        return new SaleBulkPreviewRow(
                r.getReceiptNo(),
                r.getProductName(),
                r.getSku(),
                r.getQuantity(),
                r.getUnitPrice(),
                Optional.ofNullable(r.getBranchCode()).orElse("MAIN")
        );
    }

    /* ============================================================
       INTERNAL RECORDS
       ============================================================ */
    private record ValidationOutcome(
            List<PlannedReceipt> receipts
    ) {}

    private record PlannedReceipt(
            String receiptNo,
            LocalDateTime saleDate,
            List<PlannedLineItem> items,
            String paymentsRaw,
            BigDecimal total
    ) {}

    private record PlannedLineItem(
            ProductVariant variant,
            Branch branch,
            ProductVariantPackaging packaging,
            long quantity,
            long baseUnits,
            BigDecimal unitPrice,
            BigDecimal netAmount,
            BigDecimal vatAmount,
            BigDecimal vatRate, // ✅ ADD
            BigDecimal lineTotal,
            String pricingJson
    ) {}
}