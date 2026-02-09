package com.IntegrityTechnologies.business_manager.modules.finance.sales.service;

import com.IntegrityTechnologies.business_manager.common.bulk.*;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.service.PaymentService;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.dto.*;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.SaleLineItem;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.repository.SaleRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.interceptor.TransactionAspectSupport;

import java.math.BigDecimal;
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

                if (!inventoryService.inventoryExists(
                        variant.getId(), branch.getId())) {
                    throw new IllegalArgumentException(
                            "No inventory for " +
                                    variant.getProduct().getName() +
                                    " (" + variant.getClassification() + ")" +
                                    " in branch " + branch.getBranchCode()
                    );
                }

                long available =
                        inventoryService.availableQuantity(
                                variant.getId(), branch.getId());

                if (available < row.getQuantity()) {
                    throw new IllegalArgumentException(
                            "Insufficient stock for " +
                                    variant.getProduct().getName() +
                                    " (" + variant.getClassification() + ")" +
                                    " in branch " + branch.getBranchCode() +
                                    " (available=" + available + ")"
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
        List<PlannedLineItem> items = new ArrayList<>();

        for (SaleBulkRow row : rows) {
            ProductVariant variant = resolveVariant(row);

            BigDecimal unitPrice =
                    Optional.ofNullable(row.getUnitPrice())
                            .orElse(variant.getMinimumSellingPrice());

            BigDecimal lineTotal =
                    unitPrice.multiply(BigDecimal.valueOf(row.getQuantity()));

            items.add(
                    new PlannedLineItem(
                            variant,
                            branch,
                            row.getQuantity(),
                            unitPrice,
                            lineTotal
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

            Sale sale = Sale.builder()
                    .id(UUID.randomUUID())
                    .createdAt(plan.saleDate())
                    .createdBy(currentUser)
                    .receiptNo(plan.receiptNo())
                    .totalAmount(plan.total())
                    .totalDiscount(BigDecimal.ZERO)
                    .totalTax(BigDecimal.ZERO)
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
                                .quantity(li.quantity())
                                .unitPrice(li.unitPrice())
                                .lineTotal(li.lineTotal())
                                .build()
                );
            }

            saleRepository.save(sale);

            if (mode == SaleImportMode.OPERATIONAL) {
                for (PlannedLineItem li : plan.items()) {
                    inventoryService.reserveStockVariant(
                            li.variant().getId(),
                            li.branch().getId(),
                            li.quantity(),
                            "BULK-SALE:" + plan.receiptNo()
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

        if (row.getSku() != null && !row.getSku().isBlank()) {
            ProductVariant v =
                    productVariantRepository
                            .findBySku(row.getSku().trim())
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
                    productVariantRepository
                            .findByProduct_NameIgnoreCaseAndClassificationAndDeletedFalse(
                                    row.getProductName().trim(),
                                    classification
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

        return branchRepository.findByBranchCode(code)
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
            int quantity,
            BigDecimal unitPrice,
            BigDecimal lineTotal
    ) {}
}