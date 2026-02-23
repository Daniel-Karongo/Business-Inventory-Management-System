package com.IntegrityTechnologies.business_manager.modules.stock.inventory.service;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import com.IntegrityTechnologies.business_manager.exception.OutOfStockException;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.InventoryAccountingAdapter;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.service.PeriodGuardService;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.SaleLineBatchSelection;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.config.TaxProperties;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.repository.SupplierRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.category.controller.CategoryController;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.stock.category.repository.CategoryRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.*;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductAudit;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service.ProductService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.service.ProductVariantService;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import jakarta.persistence.OptimisticLockException;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;

@Service
@RequiredArgsConstructor
public class InventoryService {

    private final InventoryItemRepository inventoryItemRepository;
    private final StockTransactionRepository stockTransactionRepository;
    private final ProductRepository productRepository;
    private final ProductVariantRepository productVariantRepository;
    private final SupplierRepository supplierRepository;
    private final BranchRepository branchRepository;
    private final ProductAuditRepository productAuditRepository;
    private final CategoryRepository categoryRepository;
    private final InventorySnapshotRepository inventorySnapshotRepository;
    private final ProductService productService;
    private final InventoryBatchRepository batchRepository;
    private final TaxProperties taxProperties;
    private final PeriodGuardService periodGuardService;

    private static final int MAX_RETRIES = 5;
    private final ProductVariantService productVariantService;
    private final InventoryAccountingAdapter inventoryAccountingPort;
    private final AccountingFacade accountingFacade;
    private final AccountingAccounts accountingAccounts;
    private final BatchConsumptionRepository batchConsumptionRepository;

    // ------------------------
    // EXISTING / CORE METHODS
    // ------------------------

    @Transactional
    public ApiResponse receiveStock(ReceiveStockRequest req) {
        periodGuardService.validateOpenPeriod(LocalDate.now());
        // -------------------------------------------------------------
        // 0. VALIDATION
        // -------------------------------------------------------------
        if (req.getProductId() == null)
            throw new IllegalArgumentException("productId is required");

        if (req.getBranchId() == null)
            throw new IllegalArgumentException("branchId is required");

        if (req.getSuppliers() == null || req.getSuppliers().isEmpty())
            throw new IllegalArgumentException("At least one supplier must be provided");


        // -------------------------------------------------------------
        // 1. RESOLVE PRODUCT + BRANCH
        // -------------------------------------------------------------
        Product product = productRepository.findById(req.getProductId())
                .orElseThrow(() -> new IllegalArgumentException("Product not found"));

        Branch branch = branchRepository.findById(req.getBranchId())
                .orElseThrow(() -> new IllegalArgumentException("Branch not found"));


        // -------------------------------------------------------------
        // 2. RESOLVE OR CREATE VARIANT
        // -------------------------------------------------------------
        ProductVariant variant;

        if (req.getProductVariantId() != null) {
            variant = productVariantRepository.findById(req.getProductVariantId())
                    .orElseThrow(() -> new IllegalArgumentException("Variant not found"));
        } else {

            if (req.getClassification() == null || req.getClassification().isBlank())
                throw new IllegalArgumentException("classification is required when no productVariantId provided");

            variant = productVariantRepository.findByProductIdAndClassification(
                    req.getProductId(), req.getClassification()
            ).orElse(null);

            if (variant == null) {
                variant = new ProductVariant();
                variant.setProduct(product);
                variant.setClassification(req.getClassification());

                variant.setSku(
                        req.getNewVariantSku() != null ?
                                req.getNewVariantSku() :
                                productService.generateVariantSku(product, req.getClassification())
                );

                variant = productVariantRepository.save(variant); // persist immediately
            }
        }

        if (variant.getId() == null) {
            variant = productVariantRepository.save(variant);
        }


        // -------------------------------------------------------------
// 3. COMPUTE INCOMING COST + UNITS (VAT-AWARE)
// -------------------------------------------------------------
        long incomingUnits = 0;
        BigDecimal incomingCostTotal = BigDecimal.ZERO;
        BigDecimal totalInputVat = BigDecimal.ZERO;

        boolean vatEnabled = taxProperties.isVatEnabled();
        BigDecimal systemVatRate = taxProperties.getVatRate();
        boolean defaultInclusive = taxProperties.isPricesVatInclusive();

        boolean vatInclusive = req.getVatInclusive() != null
                ? req.getVatInclusive()
                : defaultInclusive;

        BigDecimal vatRate = req.getVatRate() != null
                ? req.getVatRate()
                : systemVatRate;

        Set<Supplier> suppliersUsed = new HashSet<>();

        for (SupplierUnit su : req.getSuppliers()) {

            if (su.getUnitsSupplied() == null || su.getUnitsSupplied() <= 0)
                throw new IllegalArgumentException("unitsSupplied must be > 0");

            if (su.getUnitCost() == null)
                throw new IllegalArgumentException("unitCost must be provided");

            BigDecimal qty = BigDecimal.valueOf(su.getUnitsSupplied());
            BigDecimal unitCost = su.getUnitCost();

            BigDecimal gross = unitCost.multiply(qty);

            BigDecimal net;
            BigDecimal vat;

            if (vatEnabled) {

                if (vatInclusive) {
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

            incomingUnits += su.getUnitsSupplied();
            incomingCostTotal = incomingCostTotal.add(net);
            totalInputVat = totalInputVat.add(vat);

            supplierRepository.findByIdAndDeletedFalse(su.getSupplierId())
                    .ifPresent(suppliersUsed::add);
        }

        if (incomingUnits == 0)
            throw new IllegalArgumentException("Total units supplied must be > 0");

        BigDecimal incomingAvgCost = incomingCostTotal.divide(
                BigDecimal.valueOf(incomingUnits),
                6, RoundingMode.HALF_UP
        );


        // -------------------------------------------------------------
        // 4. LOAD OR CREATE INVENTORY ITEM
        // -------------------------------------------------------------
        InventoryItem item = inventoryItemRepository
                .findByProductVariant_IdAndBranchId(variant.getId(), req.getBranchId())
                .orElse(null);

        if (item == null) {
            item = InventoryItem.builder()
                    .productId(product.getId())
                    .productVariant(variant)
                    .branch(branch)
                    .quantityOnHand(0L)
                    .quantityReserved(0L)
                    .averageCost(BigDecimal.ZERO)
                    .build();
        }

        long oldQty = item.getQuantityOnHand();
        BigDecimal oldAvgCost = item.getAverageCost() == null ? BigDecimal.ZERO : item.getAverageCost();


        // -------------------------------------------------------------
        // 5. COMPUTE NEW WEIGHTED AVERAGE COST
        // -------------------------------------------------------------
        long newQty = oldQty + incomingUnits;
        BigDecimal newAvgCost;

        if (newQty == 0) {
            newAvgCost = BigDecimal.ZERO;
        } else {
            BigDecimal totalOldValue = oldAvgCost.multiply(BigDecimal.valueOf(oldQty));
            BigDecimal totalIncomingValue = incomingAvgCost.multiply(BigDecimal.valueOf(incomingUnits));

            newAvgCost = totalOldValue.add(totalIncomingValue)
                    .divide(BigDecimal.valueOf(newQty), 6, RoundingMode.HALF_UP);
        }

        item.setQuantityOnHand(newQty);
        item.setAverageCost(newAvgCost);
        item.setLastUpdatedAt(LocalDateTime.now());
        item.setLastUpdatedBy(getCurrentUsername());

        inventoryItemRepository.save(item);

        // -------------------------------------------------------------
        // CREATE FIFO BATCHES (PER SUPPLIER ENTRY)
        // -------------------------------------------------------------
        for (SupplierUnit su : req.getSuppliers()) {

            InventoryBatch batch = InventoryBatch.builder()
                    .productVariantId(variant.getId())
                    .branchId(branch.getId())
                    .unitCost(su.getUnitCost())
                    .quantityReceived(su.getUnitsSupplied())
                    .quantityRemaining(su.getUnitsSupplied())
                    .receivedAt(LocalDateTime.now())
                    .build();

            batchRepository.save(batch);
        }

        // -------------------------------------------------------------
        // 6. UPDATE VARIANT PRICING
        // -------------------------------------------------------------
        Double margin = product.getMinimumPercentageProfit();
        if (margin == null) margin = 0D;

        variant.setAverageBuyingPrice(newAvgCost);

        // default calculated selling price
        variant.setMinimumSellingPrice(
                productVariantService.computeMinSelling(newAvgCost, margin)
        );

        // 🔐 OPTIONAL OVERRIDE FROM UI
        if (req.getSellingPrice() != null) {
            variant.setMinimumSellingPrice(req.getSellingPrice());
        }

        productVariantRepository.save(variant);


        // -------------------------------------------------------------
        // 7. RECORD STOCK TRANSACTIONS
        // -------------------------------------------------------------
        for (SupplierUnit su : req.getSuppliers()) {
            StockTransaction txn = StockTransaction.builder()
                    .productId(product.getId())
                    .productVariantId(variant.getId())
                    .branchId(req.getBranchId())
                    .type(StockTransaction.TransactionType.RECEIPT)
                    .quantityDelta(su.getUnitsSupplied())
                    .unitCost(su.getUnitCost())
                    .reference(req.getReference())
                    .supplierId(su.getSupplierId())
                    .note(req.getNote())
                    .timestamp(LocalDateTime.now())
                    .performedBy(getCurrentUsername())
                    .build();

            stockTransactionRepository.save(txn);
        }


        // -------------------------------------------------------------
        // 8. UPDATE PRODUCT SUPPLIERS & AUDIT
        // -------------------------------------------------------------
        Category category = product.getCategory();
        Set<Supplier> addedSuppliers = new HashSet<>();

        for (Supplier s : suppliersUsed) {
            if (!product.getSuppliers().contains(s)) {
                product.getSuppliers().add(s);
                addedSuppliers.add(s);
            }

            if (category != null && !category.getSuppliers().contains(s)) {
                category.getSuppliers().add(s);
            }
        }

        productRepository.save(product);
        if (category != null) categoryRepository.save(category);

        if (!addedSuppliers.isEmpty()) {
            createProductAudit(
                    product,
                    "UPDATE",
                    "suppliers",
                    null,
                    addedSuppliers.stream()
                            .map(s -> s.getId() + " - " + s.getName())
                            .toList()
                            .toString()
            );
        }

        if (vatEnabled && totalInputVat.compareTo(BigDecimal.ZERO) > 0) {

            accountingFacade.post(
                    AccountingEvent.builder()
                            .sourceModule("INVENTORY_RECEIPT")
                            .branchId(req.getBranchId())
                            .sourceId(variant.getId())
                            .reference(req.getReference())
                            .description("Inventory receipt with VAT")
                            .performedBy(getCurrentUsername())
                            .entries(List.of(
                                    AccountingEvent.Entry.builder()
                                            .accountId(accountingAccounts.inventory())
                                            .direction(EntryDirection.DEBIT)
                                            .amount(incomingCostTotal)
                                            .build(),

                                    AccountingEvent.Entry.builder()
                                            .accountId(accountingAccounts.inputVat())
                                            .direction(EntryDirection.DEBIT)
                                            .amount(totalInputVat)
                                            .build(),

                                    AccountingEvent.Entry.builder()
                                            .accountId(accountingAccounts.accountsPayable())
                                            .direction(EntryDirection.CREDIT)
                                            .amount(incomingCostTotal.add(totalInputVat))
                                            .build()
                            ))
                            .build()
            );

        } else {

            inventoryAccountingPort.recordInventoryReceipt(
                    variant.getId(),
                    req.getBranchId(),
                    incomingCostTotal,
                    req.getReference()
            );
        }

        // -------------------------------------------------------------
        // 9. RETURN RESPONSE
        // -------------------------------------------------------------
        return new ApiResponse("success", "Stock received successfully", buildResponse(item));
    }

    @Transactional(readOnly = true)
    public InventoryBulkPreviewRow simulateReceive(
            ReceiveStockRequest req
    ) {

    /* =========================
       1. RESOLVE PRODUCT + BRANCH
       ========================= */

        Product product = productRepository.findById(req.getProductId())
                .orElseThrow(() ->
                        new IllegalArgumentException("Product not found")
                );

        Branch branch = branchRepository.findById(req.getBranchId())
                .orElseThrow(() ->
                        new IllegalArgumentException("Branch not found")
                );

    /* =========================
       2. RESOLVE OR SIMULATE VARIANT
       ========================= */

        ProductVariant variant =
                req.getProductVariantId() != null
                        ? productVariantRepository
                        .findById(req.getProductVariantId())
                        .orElseThrow(() ->
                                new IllegalArgumentException("Variant not found")
                        )
                        : productVariantRepository
                        .findByProductIdAndClassification(
                                req.getProductId(),
                                req.getClassification()
                        )
                        .orElseGet(() -> {
                            ProductVariant v = new ProductVariant();
                            v.setProduct(product);
                            v.setClassification(req.getClassification());
                            v.setSku(
                                    req.getNewVariantSku() != null
                                            ? req.getNewVariantSku()
                                            : productService.generateVariantSku(
                                            product,
                                            req.getClassification()
                                    )
                            );
                            return v; // ❗ NOT persisted
                        });

    /* =========================
       3. COMPUTE UNITS + COST
       ========================= */

        long totalUnits = 0;
        BigDecimal totalCost = BigDecimal.ZERO;

        for (SupplierUnit su : req.getSuppliers()) {

            if (su.getUnitsSupplied() == null || su.getUnitsSupplied() <= 0)
                throw new IllegalArgumentException("unitsSupplied must be > 0");

            if (su.getUnitCost() == null)
                throw new IllegalArgumentException("unitCost is required");

            totalUnits += su.getUnitsSupplied();

            totalCost = totalCost.add(
                    su.getUnitCost().multiply(
                            BigDecimal.valueOf(su.getUnitsSupplied())
                    )
            );
        }

        if (totalUnits == 0)
            throw new IllegalArgumentException("Total units must be > 0");

        BigDecimal unitCost =
                totalCost.divide(
                        BigDecimal.valueOf(totalUnits),
                        6,
                        RoundingMode.HALF_UP
                );

    /* =========================
       4. PREVIEW RESULT
       ========================= */

        return InventoryBulkPreviewRow.builder()
                .productId(product.getId())
                .productName(product.getName())

                .productVariantId(variant.getId())
                .variantClassification(variant.getClassification())
                .variantSku(variant.getSku())

                .branchId(branch.getId())
                .branchName(branch.getName())

                .unitsReceived(totalUnits)
                .unitCost(unitCost)
                .sellingPrice(req.getSellingPrice())
                .totalCost(totalCost)
                .build();
    }

    @Transactional
    public ApiResponse transferStock(TransferStockRequest req) {
        periodGuardService.validateOpenPeriod(LocalDate.now());

        if (req.getProductVariantId() == null)
            throw new IllegalArgumentException("productVariantId is required");

        if (req.getFromBranchId() == null || req.getToBranchId() == null)
            throw new IllegalArgumentException("Both fromBranchId and toBranchId are required");

        if (req.getFromBranchId().equals(req.getToBranchId()))
            throw new IllegalArgumentException("Source and destination branch cannot be the same");

        if (req.getQuantity() <= 0)
            throw new IllegalArgumentException("Quantity must be > 0");

        return processTransfer(req);
    }

    private ApiResponse processTransfer(TransferStockRequest req) {

        UUID variantId = req.getProductVariantId();
        UUID fromBranch = req.getFromBranchId();
        UUID toBranch = req.getToBranchId();
        long quantity = req.getQuantity();

        List<InventoryBatch> sourceBatches =
                batchRepository
                        .findByProductVariantIdAndBranchIdAndQuantityRemainingGreaterThanOrderByReceivedAtAsc(
                                variantId,
                                fromBranch,
                                0L
                        );

        long remaining = quantity;

        for (InventoryBatch sourceBatch : sourceBatches) {

            if (remaining <= 0) break;

            long moveQty = Math.min(sourceBatch.getQuantityRemaining(), remaining);

            sourceBatch.setQuantityRemaining(
                    sourceBatch.getQuantityRemaining() - moveQty
            );

            // CREATE NEW BATCH IN DESTINATION
            InventoryBatch newBatch = InventoryBatch.builder()
                    .productVariantId(variantId)
                    .branchId(toBranch)
                    .unitCost(sourceBatch.getUnitCost())
                    .quantityReceived(moveQty)
                    .quantityRemaining(moveQty)
                    .receivedAt(sourceBatch.getReceivedAt())
                    .build();

            batchRepository.save(newBatch);

            remaining -= moveQty;
        }

        if (remaining > 0)
            throw new OutOfStockException("Insufficient stock for transfer");

        // Update inventory quantities
        decrementPhysicalOnly(variantId, fromBranch, quantity);
        incrementPhysicalOnly(variantId, toBranch, quantity);

        return new ApiResponse("success", "Batch-aware transfer complete");
    }

    private void decrementPhysicalOnly(UUID variantId, UUID branchId, long qty) {

        InventoryItem item = inventoryItemRepository
                .findByProductVariant_IdAndBranchId(variantId, branchId)
                .orElseThrow();

        item.setQuantityOnHand(item.getQuantityOnHand() - qty);
        inventoryItemRepository.save(item);
    }

    private void incrementPhysicalOnly(UUID variantId, UUID branchId, long qty) {

        InventoryItem item = inventoryItemRepository
                .findByProductVariant_IdAndBranchId(variantId, branchId)
                .orElseThrow();

        item.setQuantityOnHand(item.getQuantityOnHand() + qty);
        inventoryItemRepository.save(item);
    }

    @Transactional
    public void restoreConsumedBatches(UUID saleId, UUID branchId, UUID variantId) {

        List<BatchConsumption> consumptions =
                batchConsumptionRepository.findBySaleIdAndProductVariantId(
                        saleId,
                        variantId
                );

        for (BatchConsumption bc : consumptions) {

            InventoryBatch batch = batchRepository.findById(bc.getBatchId())
                    .orElseThrow();

            batch.setQuantityRemaining(
                    batch.getQuantityRemaining() + bc.getQuantity()
            );
        }

        long totalRestore = consumptions.stream()
                .mapToLong(BatchConsumption::getQuantity)
                .sum();

        incrementPhysicalOnly(variantId, branchId, totalRestore);
    }

    // ------------------------
    // VARIANT RESERVE / RELEASE / DECREMENT (existing)
    // ------------------------
    @Transactional
    public void reserveStockVariant(UUID productVariantId, UUID branchId, int quantity, String reference) {
        if (quantity <= 0) throw new IllegalArgumentException("Quantity must be > 0");

        int attempts = 0;
        while (true) {
            try {
                processReservationVariant(productVariantId, branchId, quantity, reference);
                return;
            } catch (OptimisticLockException e) {
                attempts++;
                if (attempts >= MAX_RETRIES) {
                    throw new RuntimeException("Failed to reserve stock due to concurrent updates.");
                }
            }
        }
    }

    private void processReservationVariant(UUID productVariantId, UUID branchId, long quantity, String reference) {
        InventoryItem item = inventoryItemRepository.findByProductVariant_IdAndBranchId(productVariantId, branchId)
                .orElseThrow(() -> new IllegalArgumentException("Inventory item not found"));

        long available = item.getQuantityOnHand() - item.getQuantityReserved();
        if (available < quantity) {
            throw new OutOfStockException("Not enough stock to reserve");
        }

        item.setQuantityReserved(item.getQuantityReserved() + quantity);
        item.setLastUpdatedAt(LocalDateTime.now());
        item.setLastUpdatedBy(getCurrentUsername());
        inventoryItemRepository.save(item);

        stockTransactionRepository.save(StockTransaction.builder()
                .productId(item.getProductVariant().getProduct().getId())
                .productVariantId(productVariantId)
                .branchId(branchId)
                .type(StockTransaction.TransactionType.RESERVATION)
                .quantityDelta(quantity)
                .reference(reference)
                .note("Reserved for order")
                .timestamp(LocalDateTime.now())
                .performedBy(getCurrentUsername())
                .build()
        );
    }

    @Transactional
    public void releaseReservationVariant(UUID productVariantId, UUID branchId, int quantity, String reference) {
        if (quantity <= 0) throw new IllegalArgumentException("Quantity must be > 0");

        int attempts = 0;

        while (true) {
            try {
                processReleaseReservation(productVariantId, branchId, quantity, reference);
                return;
            } catch (OptimisticLockException ex) {
                attempts++;
                if (attempts >= MAX_RETRIES) {
                    throw new RuntimeException("Failed to release reservation due to concurrent modification.", ex);
                }
            }
        }
    }

    private void processReleaseReservation(UUID productVariantId, UUID branchId, int quantity, String reference) {
        InventoryItem item = inventoryItemRepository.findByProductVariant_IdAndBranchId(productVariantId, branchId)
                .orElseThrow(() -> new IllegalArgumentException("Inventory item not found"));

        long releaseQty = Math.min(quantity, item.getQuantityReserved());

        if (releaseQty <= 0) {
            throw new IllegalArgumentException("Nothing to release (reserved=" + item.getQuantityReserved() + ")");
        }

        item.setQuantityReserved(item.getQuantityReserved() - releaseQty);
        item.setLastUpdatedAt(LocalDateTime.now());
        item.setLastUpdatedBy(getCurrentUsername());
        inventoryItemRepository.save(item);

        // StockTransaction insert also versioned → must be inside retry wrapper
        stockTransactionRepository.save(
                StockTransaction.builder()
                        .productId(item.getProductId())
                        .productVariantId(productVariantId)
                        .branchId(branchId)
                        .type(StockTransaction.TransactionType.RELEASE)
                        .quantityDelta(-releaseQty)
                        .reference(reference)
                        .note("Release reservation")
                        .timestamp(LocalDateTime.now())
                        .performedBy(getCurrentUsername())
                        .build()
        );
    }

    @Transactional
    public void decrementVariantStock(UUID productVariantId,
                                      UUID branchId,
                                      int quantity,
                                      String reference) {

        decrementVariantStock(
                productVariantId,
                branchId,
                quantity,
                reference,
                null   // no manual selections
        );
    }
    @Transactional
    public void decrementVariantStock(UUID productVariantId,
                                      UUID branchId,
                                      int quantity,
                                      String reference,
                                      List<SaleLineBatchSelection> manualSelections) {

        if (quantity <= 0)
            throw new IllegalArgumentException("Quantity must be > 0");

        int attempts = 0;

        while (true) {
            try {
                processStockDecrementVariant(
                        productVariantId,
                        branchId,
                        quantity,
                        reference,
                        manualSelections
                );
                return;
            } catch (OptimisticLockException e) {
                attempts++;
                if (attempts >= MAX_RETRIES) {
                    throw new RuntimeException("Concurrent stock update conflict. Try again.");
                }
            }
        }
    }

    private void processStockDecrementVariant(UUID productVariantId,
                                              UUID branchId,
                                              long qty,
                                              String reference,
                                              List<SaleLineBatchSelection> manualSelections) {
        periodGuardService.validateOpenPeriod(LocalDate.now());

        InventoryItem item = inventoryItemRepository
                .findByProductVariant_IdAndBranchId(productVariantId, branchId)
                .orElseThrow(() -> new OutOfStockException("Inventory record not found"));

        long available = item.getQuantityOnHand() - item.getQuantityReserved();
        if (available < qty) {
            throw new OutOfStockException("Insufficient stock in this branch");
        }

        long remaining = qty;
        BigDecimal totalCost = BigDecimal.ZERO;

        // ======================================================
        // 1️⃣ MANUAL SELECTION
        // ======================================================
        if (manualSelections != null && !manualSelections.isEmpty()) {

            for (SaleLineBatchSelection sel : manualSelections) {

                InventoryBatch batch = batchRepository.findById(sel.getBatchId())
                        .orElseThrow(() ->
                                new IllegalArgumentException("Batch not found"));

                if (!batch.getBranchId().equals(branchId))
                    throw new IllegalStateException("Batch not in this branch");

                if (!batch.getProductVariantId().equals(productVariantId))
                    throw new IllegalStateException("Batch mismatch with variant");

                if (batch.getQuantityRemaining() < sel.getQuantity())
                    throw new OutOfStockException("Insufficient in selected batch");

                batch.setQuantityRemaining(
                        batch.getQuantityRemaining() - sel.getQuantity()
                );

                BigDecimal cost =
                        batch.getUnitCost()
                                .multiply(BigDecimal.valueOf(sel.getQuantity()));

                totalCost = totalCost.add(cost);

                batchConsumptionRepository.save(
                        BatchConsumption.builder()
                                .batchId(batch.getId())
                                .saleId(extractSaleIdFromReference(reference))
                                .productVariantId(productVariantId)
                                .quantity(sel.getQuantity())
                                .unitCost(batch.getUnitCost())
                                .build()
                );

                remaining -= sel.getQuantity();
            }
        }

// 🔥 ALWAYS run FIFO for remaining > 0
        if (remaining > 0) {

            List<InventoryBatch> batches =
                    batchRepository
                            .findByProductVariantIdAndBranchIdAndQuantityRemainingGreaterThanOrderByReceivedAtAsc(
                                    productVariantId,
                                    branchId,
                                    0L
                            );

            for (InventoryBatch batch : batches) {

                if (remaining <= 0) break;

                long deduct = Math.min(batch.getQuantityRemaining(), remaining);

                batch.setQuantityRemaining(batch.getQuantityRemaining() - deduct);

                BigDecimal cost =
                        batch.getUnitCost()
                                .multiply(BigDecimal.valueOf(deduct));

                totalCost = totalCost.add(cost);

                batchConsumptionRepository.save(
                        BatchConsumption.builder()
                                .batchId(batch.getId())
                                .saleId(extractSaleIdFromReference(reference))
                                .productVariantId(productVariantId)
                                .quantity(deduct)
                                .unitCost(batch.getUnitCost())
                                .build()
                );

                remaining -= deduct;
            }

            if (remaining > 0)
                throw new OutOfStockException("Insufficient FIFO stock");
        }

        // ======================================================
        // 2️⃣ FIFO FALLBACK
        // ======================================================
        else {

            List<InventoryBatch> batches =
                    batchRepository
                            .findByProductVariantIdAndBranchIdAndQuantityRemainingGreaterThanOrderByReceivedAtAsc(
                                    productVariantId,
                                    branchId,
                                    0L
                            );

            for (InventoryBatch batch : batches) {

                if (remaining <= 0) break;

                long deduct = Math.min(batch.getQuantityRemaining(), remaining);

                batch.setQuantityRemaining(batch.getQuantityRemaining() - deduct);

                BigDecimal cost =
                        batch.getUnitCost()
                                .multiply(BigDecimal.valueOf(deduct));

                totalCost = totalCost.add(cost);

                batchConsumptionRepository.save(
                        BatchConsumption.builder()
                                .batchId(batch.getId())
                                .saleId(extractSaleIdFromReference(reference))
                                .productVariantId(productVariantId)
                                .quantity(deduct)
                                .unitCost(batch.getUnitCost())
                                .build()
                );

                remaining -= deduct;
            }

            if (remaining > 0)
                throw new OutOfStockException("Insufficient FIFO stock");
        }

        // -----------------------------------------
        // UPDATE INVENTORY ITEM
        // -----------------------------------------
        item.setQuantityOnHand(item.getQuantityOnHand() - qty);
        item.setLastUpdatedAt(LocalDateTime.now());
        item.setLastUpdatedBy(getCurrentUsername());

        inventoryItemRepository.save(item);

        // -----------------------------------------
        // ACCOUNTING — TRUE FIFO COGS
        // -----------------------------------------
        inventoryAccountingPort.recordInventoryConsumption(
                productVariantId,
                branchId,
                totalCost,
                reference
        );

        // -----------------------------------------
        // STOCK TRANSACTION
        // -----------------------------------------
        stockTransactionRepository.save(
                StockTransaction.builder()
                        .productId(item.getProductVariant().getProduct().getId())
                        .productVariantId(productVariantId)
                        .branchId(branchId)
                        .type(StockTransaction.TransactionType.SALE)
                        .quantityDelta(-qty)
                        .reference(reference)
                        .note("Sale decrement (FIFO)")
                        .timestamp(LocalDateTime.now())
                        .performedBy(getCurrentUsername())
                        .build()
        );
    }

    private UUID extractSaleIdFromReference(String reference) {
        if (reference != null && reference.startsWith("SALE_DELIVERY:")) {
            return UUID.fromString(reference.replace("SALE_DELIVERY:", ""));
        }
        return null;
    }

    // ------------------------
    // NEW: variant-aware adjustStockVariant
    // ------------------------
    @Transactional
    public ApiResponse adjustStockVariant(AdjustStockRequest req) {
        periodGuardService.validateOpenPeriod(LocalDate.now());

        if (req.getProductVariantId() == null)
            throw new IllegalArgumentException("productVariantId is required");
        if (req.getBranchId() == null)
            throw new IllegalArgumentException("branchId is required");

        int attempts = 0;

        while (true) {
            try {
                return processAdjustStockVariant(req);
            } catch (OptimisticLockException ex) {
                attempts++;
                if (attempts >= MAX_RETRIES) {
                    throw new RuntimeException("Failed to adjust stock due to concurrent modification.", ex);
                }
            }
        }
    }

    private ApiResponse processAdjustStockVariant(AdjustStockRequest req) {
        ProductVariant variant = productVariantRepository.findById(req.getProductVariantId())
                .orElseThrow(() -> new IllegalArgumentException("Variant not found"));

        Branch branch = branchRepository.findById(req.getBranchId())
                .orElseThrow(() -> new IllegalArgumentException("Branch not found"));

        InventoryItem item = inventoryItemRepository.findByProductVariant_IdAndBranchId(
                        req.getProductVariantId(), req.getBranchId())
                .orElseThrow(() -> new IllegalArgumentException("Inventory item not found"));

        long delta = req.getQuantityDelta();
        long newQty = Math.max(0L, item.getQuantityOnHand() + delta);

        item.setQuantityOnHand(newQty);
        item.setLastUpdatedAt(LocalDateTime.now());
        item.setLastUpdatedBy(getCurrentUsername());
        inventoryItemRepository.save(item);

        stockTransactionRepository.save(
                StockTransaction.builder()
                        .productId(variant.getProduct().getId())
                        .productVariantId(variant.getId())
                        .branchId(branch.getId())
                        .type(StockTransaction.TransactionType.ADJUSTMENT)
                        .quantityDelta(delta)
                        .reference(req.getReference())
                        .note(req.getReason())
                        .timestamp(LocalDateTime.now())
                        .performedBy(getCurrentUsername())
                        .build()
        );

        return new ApiResponse("success", "Stock adjusted", buildResponse(item));
    }

    @Transactional
    public void incrementVariantStock(UUID variantId, UUID branchId, long qty, String reference) {
        if (qty <= 0) throw new IllegalArgumentException("Quantity must be > 0");

        int attempts = 0;

        while (true) {
            try {
                processIncrementVariantStock(variantId, branchId, qty, reference);
                return;
            } catch (OptimisticLockException ex) {
                attempts++;
                if (attempts >= MAX_RETRIES) {
                    throw new RuntimeException("Failed to increment stock due to concurrent modification.", ex);
                }
            }
        }
    }

    private void processIncrementVariantStock(UUID variantId, UUID branchId, long qty, String reference) {

        InventoryItem item = inventoryItemRepository
                .findByProductVariant_IdAndBranchId(variantId, branchId)
                .orElseThrow(() -> new IllegalArgumentException(
                        "Inventory entry not found for variant=" + variantId + " branch=" + branchId));

        long newQty = item.getQuantityOnHand() + qty;
        item.setQuantityOnHand(newQty);
        item.setLastUpdatedAt(LocalDateTime.now());
        item.setLastUpdatedBy(getCurrentUsername());

        inventoryItemRepository.save(item);

        BigDecimal value =
                item.getAverageCost().multiply(BigDecimal.valueOf(qty));

        inventoryAccountingPort.recordInventoryReturn(
                variantId,
                branchId,
                value,
                reference
        );

        stockTransactionRepository.save(
                StockTransaction.builder()
                        .productVariantId(variantId)
                        .productId(item.getProductId())
                        .branchId(branchId)
                        .quantityDelta(qty)
                        .unitCost(item.getAverageCost())
                        .type(StockTransaction.TransactionType.RETURN)
                        .reference(reference)
                        .timestamp(LocalDateTime.now())
                        .performedBy(getCurrentUsername())
                        .build()
        );
    }

    // ------------------------
    // READ HELPERS (variant-aware)
    // ------------------------

    public List<InventoryResponse> getAllInventory() {
        List<InventoryItem> items = inventoryItemRepository.findAll();
        List<InventoryResponse> res = new ArrayList<>();
        for (InventoryItem it : items) {
            res.add(buildResponse(it));
        }
        return res;
    }

    public List<InventoryResponse> getInventoryByBranch(UUID branchId) {
        List<InventoryItem> items = inventoryItemRepository.findByBranchId(branchId);
        List<InventoryResponse> res = new ArrayList<>();
        for (InventoryItem it : items) {
            res.add(buildResponse(it));
        }
        return res;
    }

    /**
     * Return InventoryResponse for variant+branch.
     * If branchId == null, return list across branches for the variant.
     */
    public ApiResponse getInventoryForVariantBranch(UUID productVariantId, UUID branchId) {

        if (productVariantId == null) {
            throw new IllegalArgumentException("productVariantId is required");
        }

        if (branchId != null) {
            Optional<InventoryItem> itemOpt = inventoryItemRepository.findByProductVariant_IdAndBranchId(productVariantId, branchId);
            if (itemOpt.isPresent()) {
                return new ApiResponse("success", "Inventory for variant+branch", buildResponse(itemOpt.get()));
            } else {
                return new ApiResponse("success", "Inventory for variant+branch", null);
            }
        } else {
            // aggregate across branches for the variant
            List<InventoryItem> items = inventoryItemRepository.findAll()
                    .stream()
                    .filter(it -> it.getProductVariant() != null && productVariantId.equals(it.getProductVariant().getId()))
                    .toList();

            List<InventoryResponse> aggregated = items.stream().map(item -> buildResponse(item)).toList();

            return new ApiResponse("success", "Inventory for variant across branches", aggregated);
        }
    }

    /**
     * Return stock across branches aggregated by variant for a product.
     */
    public ApiResponse getProductStockAcrossBranches(UUID productId) {
        List<InventoryItem> items = inventoryItemRepository.findAll()
                .stream()
                .filter(it -> it.getProductVariant() != null &&
                        it.getProductId() != null &&
                        productId.equals(it.getProductId()))
                .toList();

        List<InventoryResponse> res = new ArrayList<>();
        for (InventoryItem it : items) {
            res.add(buildResponse(it));
        }
        return new ApiResponse("success", "Inventory for product " + productId.toString() + " across branches", res);
    }

    public ApiResponse getProductStockInBranch(UUID productId, UUID branchId) {
        List<InventoryItem> items = inventoryItemRepository.findAll()
                .stream()
                .filter(it -> it.getProductVariant() != null &&
                        it.getProductId() != null &&
                        productId.equals(it.getProductId()) && branchId.equals(it.getBranch().getId()))
                .toList();

        List<InventoryResponse> res = new ArrayList<>();
        for (InventoryItem it : items) {
            res.add(buildResponse(it));
        }
        return new ApiResponse("success", "Inventory for product " + productId.toString() + " in branch " + branchId, res);
    }

    public ApiResponse getLowStock(Long threshold) {
        var items = inventoryItemRepository.findAll();
        List<InventoryResponse> response = items.stream()
                .filter(i -> (i.getQuantityOnHand() - i.getQuantityReserved()) <= threshold)
                .map(i -> buildResponse(i)).toList();
        return new ApiResponse("success", "Product under " + threshold + " in stock", response);
    }

    public ApiResponse getOutOfStock() {
        var items = inventoryItemRepository.findAll();
        List<InventoryResponse> response = items.stream()
                .filter(i -> (i.getQuantityOnHand() - i.getQuantityReserved()) <= 0)
                .map(i -> buildResponse(i)).toList();
        return new ApiResponse("success", "Out of stock products", response);
    }

    public List<ProductAudit> getAuditTrail(UUID productId) {
        return productAuditRepository.findByProductIdOrderByTimestampDesc(productId);
    }

    // Snapshot and historical snapshot handling (kept similar to your previous logic)
    @Transactional
    public void takeSnapshot(LocalDate date) {
        List<InventoryItem> items = inventoryItemRepository.findAll();

        for (InventoryItem i : items) {

            ProductVariant variant = i.getProductVariant();

            // -----------------------------
            // Determine valuation unit
            // -----------------------------
            // Priority:
            // 1. inventory averageCost
            // 2. variant averageBuyingPrice
            // 3. zero
            BigDecimal valuationUnit =
                    Optional.ofNullable(i.getAverageCost())
                            .filter(cost -> cost.compareTo(BigDecimal.ZERO) > 0)
                            .orElse(
                                    Optional.ofNullable(variant.getAverageBuyingPrice())
                                            .orElse(BigDecimal.ZERO)
                            );

            // -----------------------------
            // Build snapshot
            // -----------------------------
            InventorySnapshot snap = InventorySnapshot.builder()
                    .productId(variant.getProduct().getId())
                    .productVariantId(variant.getId())
                    .branchId(i.getBranch().getId())

                    .quantityOnHand(i.getQuantityOnHand())
                    .quantityReserved(i.getQuantityReserved())

                    // valuation = unit valuation × quantity
                    .valuation(valuationUnit.multiply(BigDecimal.valueOf(i.getQuantityOnHand())))

                    .snapshotDate(date)
                    .createdAt(LocalDateTime.now())
                    .build();

            inventorySnapshotRepository.save(snap);
        }
    }

    public List<Map<String, Object>> getSnapshot(LocalDate date) {

        LocalDateTime toDate = date.atTime(23, 59, 59);

        List<Map<String, Object>> result = new ArrayList<>();

        List<InventoryItem> allItems = inventoryItemRepository.findAll();

        for (InventoryItem item : allItems) {
            UUID pvid = item.getProductVariant().getId();
            UUID pid = item.getProductVariant().getProduct().getId();
            UUID bid = item.getBranch().getId();

            var last = inventorySnapshotRepository.findTopByProductVariantIdAndBranchIdAndSnapshotDateLessThanEqualOrderBySnapshotDateDesc(pvid, bid, date);

            long baseOnHand = last.map(InventorySnapshot::getQuantityOnHand).orElse(0L);
            long baseReserved = last.map(InventorySnapshot::getQuantityReserved).orElse(0L);
            LocalDate baseDate = last.map(InventorySnapshot::getSnapshotDate).orElse(LocalDate.EPOCH);

            LocalDateTime baseDateTime = baseDate.atStartOfDay();

            List<StockTransaction> txns =
                    stockTransactionRepository.findBetweenVariant(pvid, bid, baseDateTime, toDate);

            long deltaOnHand = 0;
            long deltaReserved = 0;

            for (StockTransaction txn : txns) {
                switch (txn.getType()) {
                    case RECEIPT, ADJUSTMENT ->
                            deltaOnHand += txn.getQuantityDelta();

                    case SALE ->
                            baseOnHand += txn.getQuantityDelta();

                    case RESERVATION ->
                            deltaReserved += txn.getQuantityDelta();

                    case RELEASE ->
                            deltaReserved -= txn.getQuantityDelta();
                }
            }

            Map<String, Object> map = new HashMap<>();
            map.put("productId", pid);
            map.put("branchId", bid);
            map.put("quantityOnHand", baseOnHand + deltaOnHand);
            map.put("quantityReserved", baseReserved + deltaReserved);
            map.put("snapshotDate", baseDate);

            result.add(map);
        }

        return result;
    }

    @Transactional(readOnly = true)
    public boolean inventoryExists(UUID variantId, UUID branchId) {
        return inventoryItemRepository
                .findByProductVariant_IdAndBranchId(variantId, branchId)
                .isPresent();
    }

    @Transactional(readOnly = true)
    public long availableQuantity(UUID variantId, UUID branchId) {
        return inventoryItemRepository
                .findByProductVariant_IdAndBranchId(variantId, branchId)
                .map(i -> i.getQuantityOnHand() - i.getQuantityReserved())
                .orElse(0L);
    }

    // ------------------------
    // Helpers
    // ------------------------



    private InventoryResponse buildResponse(InventoryItem item) {
        return InventoryResponse.builder()
                .productId(item.getProductVariant().getProduct().getId())
                .productName(item.getProductVariant().getProduct().getName())
                .productSKU(item.getProductVariant().getProduct().getSku())
                .productVariantId(item.getProductVariant().getId())
                .productClassification(item.getProductVariant().getClassification())
                .productVariantSKU(item.getProductVariant().getSku())
                .branchId(item.getBranch().getId())
                .branchName(item.getBranch().getName())
                .averageCost(item.getAverageCost())
                .quantityOnHand(item.getQuantityOnHand())
                .quantityReserved(item.getQuantityReserved())
                .lastUpdatedAt(item.getLastUpdatedAt() != null ? item.getLastUpdatedAt().toString() : null)
                .build();
    }

    private String getCurrentUsername() {
        var auth = SecurityContextHolder.getContext().getAuthentication();
        return auth != null ? auth.getName() : "SYSTEM";
    }

    private void createProductAudit(Product product, String action, String fieldChanged, String oldValue, String newValue) {
        var createAudit = new ProductAudit();
        createAudit.setAction(action);
        createAudit.setFieldChanged(fieldChanged);
        createAudit.setOldValue(oldValue);
        createAudit.setNewValue(newValue);
        createAudit.setProductId(product.getId());
        createAudit.setProductName(product.getName());
        createAudit.setTimestamp(LocalDateTime.now());
        createAudit.setPerformedBy(SecurityUtils.currentUsername());
        productAuditRepository.save(createAudit);
    }

    @Transactional(readOnly = true)
    public List<Map<String, Object>> suggestBatches(
            UUID variantId,
            UUID branchId,
            long quantityNeeded
    ) {

        List<Map<String, Object>> result = new ArrayList<>();

        long remaining = quantityNeeded;

        List<InventoryBatch> batches =
                batchRepository
                        .findByProductVariantIdAndBranchIdAndQuantityRemainingGreaterThanOrderByReceivedAtAsc(
                                variantId,
                                branchId,
                                0L
                        );

        for (InventoryBatch batch : batches) {

            if (remaining <= 0) break;

            long allocate = Math.min(batch.getQuantityRemaining(), remaining);

            Map<String, Object> row = new HashMap<>();
            row.put("batchId", batch.getId());
            row.put("available", batch.getQuantityRemaining());
            row.put("allocated", allocate);
            row.put("unitCost", batch.getUnitCost());
            row.put("receivedAt", batch.getReceivedAt());

            result.add(row);

            remaining -= allocate;
        }

        if (remaining > 0)
            throw new OutOfStockException("Insufficient batch stock");

        return result;
    }
}