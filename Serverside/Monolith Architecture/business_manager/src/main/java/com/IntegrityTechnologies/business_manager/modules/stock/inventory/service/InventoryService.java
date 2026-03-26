package com.IntegrityTechnologies.business_manager.modules.stock.inventory.service;

import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.config.response.PageWrapper;
import com.IntegrityTechnologies.business_manager.exception.OutOfStockException;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountRole;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.service.PeriodGuardService;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.model.SaleLineBatchSelection;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto.AllocationResult;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.config.TaxProperties;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.repository.SupplierRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.CategorySupplier;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.CategorySupplierId;
import com.IntegrityTechnologies.business_manager.modules.stock.category.repository.CategoryRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.accounting.InventoryAccountingPort;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.engine.StockEngine;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.*;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.*;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.projection.VariantReservationSummary;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.valuation.InventoryValuationEngine;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.valuation.InventoryValuationService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductAudit;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductSupplier;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service.ProductService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.repository.ProductVariantRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.model.ProductVariantPackaging;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.repository.ProductVariantPackagingRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.repository.ProductPriceRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchContext;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.SecurityUtils;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.persistence.OptimisticLockException;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
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
    private final InventoryAccountingPort inventoryAccountingPort;
    private final AccountingFacade accountingFacade;
    private final AccountingAccounts accountingAccounts;
    private final BatchConsumptionRepository batchConsumptionRepository;
    private final BatchReservationRepository batchReservationRepository;
    private final BranchTenantGuard branchTenantGuard;
    private final ProductPriceRepository productPriceRepository;
    private final ProductVariantPackagingRepository packagingRepository;
    private final InventoryValuationService valuationService;
    private final InventoryValuationEngine valuationEngine;
    private final TenantInventorySettingsService settingsService;
    private final StockEngine stockEngine;

    private static final int MAX_RETRIES = 5;


    private UUID tenantId() {
        return TenantContext.getTenantId();
    }
    private UUID branchId() {
        return BranchContext.get();
    }
    
    // ------------------------
    // EXISTING / CORE METHODS
    // ------------------------

    @Transactional
    public ApiResponse receiveStock(ReceiveStockRequest req) {

        return assertNotDuplicateOrIgnore(() -> {

            branchTenantGuard.validate(req.getBranchId());

            periodGuardService.validateOpenPeriod(
                    LocalDate.now(),
                    req.getBranchId()
            );

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
            // 2. RESOLVE OR CREATE VARIANT (UNCHANGED LOGIC)
            // -------------------------------------------------------------

            if (req.getProductVariantId() == null) {
                throw new IllegalArgumentException(
                        "productVariantId is required. Use StockOnboarding flow."
                );
            }

            ProductVariant variant = productVariantRepository.findById(req.getProductVariantId())
                    .orElseThrow(() -> new IllegalArgumentException("Variant not found"));

            ProductVariantPackaging basePackaging =
                    packagingRepository.findByProductVariantIdAndIsBaseUnitTrueAndDeletedFalse(
                            variant.getId()
                    );

            if (basePackaging == null) {
                throw new IllegalStateException("Base packaging missing for variant");
            }

            boolean hasPricing = !productPriceRepository
                    .findByProductVariantIdAndPackagingIdAndDeletedFalse(
                            variant.getId(),
                            basePackaging.getId()
                    )
                    .isEmpty();

            if (!hasPricing) {
                throw new IllegalStateException(
                        "Cannot receive stock without pricing configured"
                );
            }

            // -------------------------------------------------------------
            // 3. COST + VAT COMPUTATION (UNCHANGED)
            // -------------------------------------------------------------
            long incomingUnits = 0;
            BigDecimal incomingCostTotal = BigDecimal.ZERO;
            BigDecimal totalInputVat = BigDecimal.ZERO;

            boolean vatEnabled = taxProperties.isVatEnabled();
            BigDecimal vatRate = req.getVatRate() != null
                    ? req.getVatRate()
                    : taxProperties.getVatRate();

            boolean vatInclusive = req.getVatInclusive() != null
                    ? req.getVatInclusive()
                    : taxProperties.isPricesVatInclusive();

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

                supplierRepository.findByIdSafe(su.getSupplierId(), false, tenantId(), branchId())
                        .ifPresent(suppliersUsed::add);
            }

            if (incomingUnits == 0)
                throw new IllegalArgumentException("Total units supplied must be > 0");

            // -------------------------------------------------------------
            // 4. ENSURE INVENTORY ITEM EXISTS (NO MUTATION)
            // -------------------------------------------------------------
            inventoryItemRepository
                    .lockByVariant(
                            variant.getId(),
                            tenantId(),
                            branch.getId()
                    )
                    .orElseGet(() -> inventoryItemRepository.save(
                            InventoryItem.builder()
                                    .productId(product.getId())
                                    .productVariantId(variant.getId())
                                    .branchId(branch.getId())
                                    .quantityOnHand(0L)
                                    .averageCost(BigDecimal.ZERO)
                                    .deleted(false)
                                    .build()
                    ));

            // -------------------------------------------------------------
            // 5. ENGINE-DRIVEN RECEIPT (🔥 FIXED)
            // -------------------------------------------------------------
            UUID receiptId = requireReferenceId(req.getReference(), "Receipt");

            for (SupplierUnit su : req.getSuppliers()) {

                stockEngine.adjustAndSync(
                        variant.getId(),
                        branch.getId(),
                        su.getUnitsSupplied(),
                        su.getUnitCost(),
                        receiptId
                );
            }

            recomputeWeightedAverage(variant.getId(), branch.getId());
            validateInventoryInvariant(variant.getId(), branch.getId());

            productVariantRepository.save(variant);

            // -------------------------------------------------------------
            // 7. STOCK TRANSACTIONS (UNCHANGED)
            // -------------------------------------------------------------
            for (SupplierUnit su : req.getSuppliers()) {

                StockTransaction txn = StockTransaction.builder()
                        .productId(product.getId())
                        .productVariantId(variant.getId())
                        .branchId(branch.getId())
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
            // 8. PRODUCT SUPPLIERS + CATEGORY + AUDIT (UNCHANGED)
            // -------------------------------------------------------------
            Category category = product.getCategory();
            Set<Supplier> addedSuppliers = new HashSet<>();

            for (Supplier s : suppliersUsed) {

                boolean alreadyLinked = product.getSuppliers()
                        .stream()
                        .anyMatch(ps -> ps.getSupplier().getId().equals(s.getId()));

                if (!alreadyLinked) {
                    ProductSupplier ps = ProductSupplier.builder()
                            .product(product)
                            .supplier(s)
                            .build();

                    product.getSuppliers().add(ps);
                    addedSuppliers.add(s);
                }

                if (category != null) {

                    boolean alreadyCategoryLinked = category.getCategorySuppliers()
                            .stream()
                            .anyMatch(rel -> rel.getSupplier().getId().equals(s.getId()));

                    if (!alreadyCategoryLinked) {

                        CategorySupplier relation = CategorySupplier.builder()
                                .id(new CategorySupplierId(category.getId(), s.getId()))
                                .category(category)
                                .supplier(s)
                                .build();

                        category.getCategorySuppliers().add(relation);
                    }
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

            // -------------------------------------------------------------
            // 9. ACCOUNTING (UNCHANGED)
            // -------------------------------------------------------------
            if (vatEnabled && totalInputVat.compareTo(BigDecimal.ZERO) > 0) {

                accountingFacade.post(
                        AccountingEvent.builder()
                                .eventId(UUID.randomUUID())
                                .sourceModule("INVENTORY_RECEIPT")
                                .branchId(branch.getId())
                                .sourceId(variant.getId())
                                .reference(req.getReference())
                                .description("Inventory receipt with VAT")
                                .performedBy(getCurrentUsername())
                                .entries(List.of(
                                        AccountingEvent.Entry.builder()
                                                .accountId(accountingAccounts.get(tenantId(), branch.getId(), AccountRole.INVENTORY))
                                                .direction(EntryDirection.DEBIT)
                                                .amount(incomingCostTotal)
                                                .build(),
                                        AccountingEvent.Entry.builder()
                                                .accountId(accountingAccounts.get(tenantId(), branch.getId(), AccountRole.VAT_INPUT))
                                                .direction(EntryDirection.DEBIT)
                                                .amount(totalInputVat)
                                                .build(),
                                        AccountingEvent.Entry.builder()
                                                .accountId(accountingAccounts.get(tenantId(), branch.getId(), AccountRole.VAT_PAYABLE))
                                                .direction(EntryDirection.CREDIT)
                                                .amount(incomingCostTotal.add(totalInputVat))
                                                .build()
                                ))
                                .build()
                );

            } else {

                receiptId = requireReferenceId(req.getReference(), "Receipt");

                inventoryAccountingPort.recordInventoryReceipt(
                        tenantId(),
                        receiptId, // ✅ FIXED
                        branch.getId(),
                        incomingCostTotal,
                        req.getReference()
                );
            }

            settingsService.lock(tenantId());
            
            return new ApiResponse(
                    "success",
                    "Stock received successfully",
                    buildResponse(getItemOrThrow(variant.getId(), branch.getId()))
            );

        }, req.getReference(), StockTransaction.TransactionType.RECEIPT, req.getBranchId());
    }

    @Transactional(readOnly = true)
    public InventoryBulkPreviewRow simulateReceive(
            ReceiveStockRequest req
    ) {

        branchTenantGuard.validate(req.getBranchId());

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
                        .findByTenantIdAndBranchIdAndProduct_IdAndClassification(
                                tenantId(),
                                req.getBranchId(),
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
        branchTenantGuard.validate(req.getToBranchId());
        branchTenantGuard.validate(req.getFromBranchId());

        periodGuardService.validateOpenPeriod(
                LocalDate.now(),
                req.getFromBranchId()
        );

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

        return assertNotDuplicateOrIgnore(() -> {
            final UUID tenantId = tenantId();

            UUID variantId = req.getProductVariantId();
            UUID fromBranch = req.getFromBranchId();
            UUID toBranch = req.getToBranchId();
            long quantity = req.getQuantity();

            // ======================================================
            // 🔒 1. LOCK SOURCE INVENTORY
            // ======================================================
            InventoryItem sourceItem = inventoryItemRepository
                    .lockByVariant(variantId, tenantId, fromBranch)
                    .orElseThrow(() -> new OutOfStockException("Source inventory not found"));

            long available = stockEngine.availableQuantity(variantId, fromBranch);

            if (available < quantity) {
                throw new OutOfStockException("Insufficient stock for transfer");
            }

            // ======================================================
            // 🔒 2. LOCK DESTINATION INVENTORY (CREATE IF MISSING)
            // ======================================================
            InventoryItem destinationItem = inventoryItemRepository
                    .lockByVariant(variantId, tenantId, toBranch)
                    .orElseGet(() -> {

                        InventoryItem newItem = InventoryItem.builder()
                                .productId(sourceItem.getProductId())
                                .productVariantId(variantId)
                                .branchId(toBranch)
                                .quantityOnHand(0L)
                                .averageCost(BigDecimal.ZERO)
                                .deleted(false)
                                .build();

                        return inventoryItemRepository.save(newItem);
                    });

            // ======================================================
            // 3️⃣ DELEGATE TO STOCK ENGINE (SINGLE SOURCE OF TRUTH)
            // ======================================================

            BigDecimal totalTransferValue =
                    stockEngine.transferAndSyncItem(
                            variantId,
                            fromBranch,
                            toBranch,
                            quantity,
                            req.getDestinationUnitCost()
                    );

            // ======================================================
            // 6️⃣ RECOMPUTE AVERAGES
            // ======================================================
            recomputeWeightedAverage(variantId, fromBranch);
            recomputeWeightedAverage(variantId, toBranch);

            validateInventoryInvariant(variantId, fromBranch);
            validateInventoryInvariant(variantId, toBranch);

            // ======================================================
            // 7️⃣ ACCOUNTING (UNCHANGED)
            // ======================================================

            UUID transferId = requireReferenceId(req.getReference(), "Transfer");

            inventoryAccountingPort.recordInventoryTransferOut(
                    tenantId,
                    transferId, // ✅ FIXED
                    fromBranch,
                    totalTransferValue,
                    req.getReference()
            );

            inventoryAccountingPort.recordInventoryTransferIn(
                    tenantId,
                    transferId, // ✅ FIXED
                    toBranch,
                    totalTransferValue,
                    req.getReference()
            );

            return new ApiResponse("success", "Batch-aware transfer complete");

        }, req.getReference(), StockTransaction.TransactionType.TRANSFER_OUT, req.getFromBranchId());
    }

    @Transactional
    public void restoreConsumedBatches(UUID saleId, UUID branchId, UUID variantId) {

        stockEngine.restoreAndSync(
                variantId,
                branchId,
                saleId
        );

        recomputeWeightedAverage(variantId, branchId);
        validateInventoryInvariant(variantId, branchId);
    }


    // ------------------------
    // NEW: variant-aware adjustStockVariant
    // ------------------------
    public ApiResponse adjustStockVariant(AdjustStockRequest req) {

        int attempts = 0;

        while (true) {
            try {
                return doAdjustStockVariant(req);

            } catch (OptimisticLockException e) {
                if (++attempts >= MAX_RETRIES) {
                    throw new RuntimeException("Adjust failed", e);
                }
            }
        }
    }

    @Transactional
    public ApiResponse doAdjustStockVariant(AdjustStockRequest req) {
        return processAdjustStockVariant(req);
    }

    private ApiResponse processAdjustStockVariant(AdjustStockRequest req) {
        return assertNotDuplicateOrIgnore(() -> {

            final UUID tenantId = tenantId();

            ProductVariant variant = productVariantRepository.findById(req.getProductVariantId())
                    .orElseThrow(() -> new IllegalArgumentException("Variant not found"));

            ProductVariantPackaging basePackaging =
                    packagingRepository.findByProductVariantIdAndIsBaseUnitTrueAndDeletedFalse(
                            variant.getId()
                    );

            if (basePackaging == null) {
                throw new IllegalStateException("Base packaging missing for variant");
            }

            boolean hasPricing = !productPriceRepository
                    .findByProductVariantIdAndPackagingIdAndDeletedFalse(
                            variant.getId(),
                            basePackaging.getId()
                    )
                    .isEmpty();

            if (!hasPricing) {
                throw new IllegalStateException(
                        "Cannot adjust stock without pricing configured"
                );
            }

            Branch branch = branchRepository.findById(req.getBranchId())
                    .orElseThrow(() -> new IllegalArgumentException("Branch not found"));

            long delta = req.getQuantityDelta();

            if (delta == 0)
                throw new IllegalArgumentException("Quantity delta cannot be zero");

            UUID refId = requireReferenceId(req.getReference(), "Adjustment");

            // 🔥 SINGLE SOURCE OF TRUTH
            BigDecimal totalValue =
                    stockEngine.adjustAndSync(
                            variant.getId(),
                            branch.getId(),
                            delta,
                            req.getUnitCost(),
                            refId
                    );

            // =========================
            // ACCOUNTING
            // =========================
            if (delta < 0) {
                inventoryAccountingPort.recordInventoryConsumption(
                        tenantId,
                        refId,
                        branch.getId(),
                        totalValue,
                        req.getReference()
                );
            } else {
                inventoryAccountingPort.recordInventoryReturn(
                        tenantId,
                        refId,
                        branch.getId(),
                        totalValue,
                        req.getReference()
                );
            }

            // =========================
            // RECOMPUTE + VALIDATE
            // =========================
            recomputeWeightedAverage(variant.getId(), branch.getId());
            validateInventoryInvariant(variant.getId(), branch.getId());

            // =========================
            // STOCK TRANSACTION
            // =========================
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

            InventoryItem updated = getItemOrThrow(variant.getId(), branch.getId());

            return new ApiResponse(
                    "success",
                    "Batch-aware stock adjusted",
                    buildResponse(updated)
            );

        }, req.getReference(), StockTransaction.TransactionType.ADJUSTMENT, req.getBranchId());
    }

    // ------------------------
    // READ HELPERS (variant-aware)
    // ------------------------

    public PageWrapper<InventoryResponse> getAllInventory(Pageable pageable) {

        Page<InventoryItem> page =
                inventoryItemRepository.findAllActive(
                        tenantId(),
                        pageable
                );

        List<UUID> variantIds = page.getContent().stream()
                .map(i -> i.getProductVariant().getId())
                .toList();

        Map<String, Long> reservedMap = computeReservedBulkMultiBranch(variantIds);

        return new PageWrapper<>(
                page.map(i -> buildResponse(i, reservedMap.getOrDefault(
                        buildKey(i.getProductVariant().getId(), i.getBranchId()),0L
                )))
        );
    }

    public PageWrapper<InventoryResponse> getInventoryByBranch(UUID branchId, Pageable pageable) {

        Page<InventoryItem> page =
                inventoryItemRepository.findByBranchScoped(
                        tenantId(),
                        branchId,
                        pageable
                );

        List<UUID> variantIds = page.getContent().stream()
                .map(i -> i.getProductVariant().getId())
                .toList();

        Map<String, Long> reservedMap = computeReservedBulkMultiBranch(variantIds);

        return new PageWrapper<>(
                page.map(i -> buildResponse(i, reservedMap.getOrDefault(
                        buildKey(i.getProductVariant().getId(), i.getBranchId()), 0L
                )))
        );
    }

    /**
     * Return InventoryResponse for variant+branch.
     * If branchId == null, return list across branches for the variant.
     */
    public ApiResponse getInventoryForVariantBranch(UUID productVariantId, UUID branchId) {

        if (branchId != null) {

            Optional<InventoryItem> opt =
                    inventoryItemRepository
                            .findByProductVariantIdAndTenantIdAndBranchIdAndDeletedFalse(
                                    productVariantId,
                                    tenantId(),
                                    branchId
                            );

            if (opt.isEmpty()) {
                return new ApiResponse("success", "No inventory", null);
            }

            InventoryItem item = opt.get();

            Map<String, Long> reservedMap =
                    computeReservedBulkMultiBranch(List.of(productVariantId));

            long reserved = reservedMap.getOrDefault(
                    buildKey(productVariantId, item.getBranchId()),
                    0L
            );

            return new ApiResponse(
                    "success",
                    "Inventory found",
                    buildResponse(item, reserved)
            );
        }

        // 🔥 MULTI-BRANCH (FIXED N+1)
        List<InventoryItem> items =
                inventoryItemRepository.findByProductScoped(
                        tenantId(),
                        productVariantId
                );

        if (items.isEmpty()) {
            return new ApiResponse("success", "No inventory", List.of());
        }

        List<UUID> variantIds = items.stream()
                .map(i -> i.getProductVariant().getId())
                .toList();

        Map<String, Long> reservedMap = computeReservedBulkMultiBranch(variantIds);


        List<InventoryResponse> responses = items.stream()
                .map(i -> buildResponse(
                        i,
                        reservedMap.getOrDefault(
                                buildKey(i.getProductVariant().getId(), i.getBranchId()),0L
                        )
                ))
                .toList();

        return new ApiResponse(
                "success",
                "Inventory across branches",
                responses
        );
    }

    /**
     * Return stock across branches aggregated by variant for a product.
     */
    public ApiResponse getProductStockAcrossBranches(UUID productId) {
        List<InventoryItem> items =
                inventoryItemRepository.findByProductScoped(
                        tenantId(),
                        productId
                );

        List<InventoryResponse> res = new ArrayList<>();
        List<UUID> variantIds = items.stream()
                .map(i -> i.getProductVariant().getId())
                .toList();

        Map<String, Long> reservedMap = computeReservedBulkMultiBranch(variantIds);

        for (InventoryItem it : items) {
            res.add(buildResponse(it, reservedMap.getOrDefault(it.getProductVariant().getId(), 0L)));
        }
        return new ApiResponse("success", "Inventory for product " + productId.toString() + " across branches", res);
    }

    public ApiResponse getProductStockInBranch(UUID productId, UUID branchId) {
        List<InventoryItem> items =
                inventoryItemRepository.findByProductScoped(
                                tenantId(),
                                productId
                        ).stream()
                        .filter(i -> branchId.equals(i.getBranchId()))
                        .toList();

        List<InventoryResponse> res = new ArrayList<>();
        for (InventoryItem it : items) {
            res.add(buildResponse(it));
        }
        return new ApiResponse("success", "Inventory for product " + productId.toString() + " in branch " + branchId, res);
    }

    public ApiResponse getLowStock(Long threshold, Pageable pageable) {

        Page<InventoryItem> page =
                inventoryItemRepository.findLowStock(
                        tenantId(),
                        threshold,
                        pageable
                );

        return new ApiResponse(
                "success",
                "Low stock",
                new PageWrapper<>(page.map(this::buildResponse))
        );
    }

    public ApiResponse getOutOfStock(Pageable pageable) {

        Page<InventoryItem> page =
                inventoryItemRepository.findOutOfStock(
                        tenantId(),
                        pageable
                );

        return new ApiResponse(
                "success",
                "Out of stock",
                new PageWrapper<>(page.map(this::buildResponse))
        );
    }

    public List<ProductAudit> getAuditTrail(UUID productId) {
        return productAuditRepository.findByTenantIdAndBranchIdAndProductIdOrderByTimestampDesc(tenantId(), branchId(), productId);
    }

    // Snapshot and historical snapshot handling (kept similar to your previous logic)
    @Transactional
    public void takeSnapshot(LocalDate date) {

        String method = valuationService.resolveCurrentMethod();

        int page = 0;
        Page<InventoryItem> slice;

        do {
            slice = inventoryItemRepository.findAllActive(
                    tenantId(),
                    PageRequest.of(page, 1000)
            );

            for (InventoryItem item : slice.getContent()) {

                long qty = batchRepository.sumRemainingByVariant(
                        item.getProductVariantId(),
                        tenantId(),
                        item.getBranchId()
                );

                if (qty <= 0) continue;

                UUID variantId = item.getProductVariant().getId();
                UUID branchId = item.getBranchId();

                BigDecimal valuation = valuationEngine.valuate(
                        method,
                        variantId,
                        branchId,
                        qty
                );

                BigDecimal unitCost = valuation.divide(
                        BigDecimal.valueOf(qty),
                        6,
                        RoundingMode.HALF_UP
                );

                InventorySnapshot snap = InventorySnapshot.builder()
                        .tenantId(tenantId())
                        .branchId(branchId)
                        .productId(item.getProductId())
                        .productVariantId(variantId)
                        .quantityOnHand(qty)
                        .valuation(valuation)
                        .unitCost(unitCost)
                        .valuationMethod(method)
                        .snapshotDate(date)
                        .createdAt(LocalDateTime.now())
                        .build();

                inventorySnapshotRepository.save(snap);
            }

            page++;

        } while (!slice.isLast());
    }

    public List<Map<String, Object>> getSnapshot(LocalDate date) {

        LocalDateTime toDate = date.atTime(23, 59, 59);

        List<Map<String, Object>> result = new ArrayList<>();

        Page<InventoryItem> page =
                inventoryItemRepository.findAllActive(tenantId(), Pageable.unpaged());

        List<InventoryItem> allItems = page.getContent();

        for (InventoryItem item : allItems) {
            UUID pvid = item.getProductVariant().getId();
            UUID pid = item.getProductVariant().getProduct().getId();
            UUID bid = item.getBranchId();

            var last = inventorySnapshotRepository.findTopByProductVariantIdAndBranchIdAndTenantIdAndSnapshotDateLessThanEqualOrderBySnapshotDateDesc(
                            pvid,
                            bid,
                            tenantId(),
                            date
                    );

            long baseOnHand = last.map(InventorySnapshot::getQuantityOnHand).orElse(0L);
            LocalDate baseDate = last.map(InventorySnapshot::getSnapshotDate).orElse(LocalDate.EPOCH);

            LocalDateTime baseDateTime = baseDate.atStartOfDay();

            List<StockTransaction> txns =
                    stockTransactionRepository.findBetweenVariant(
                            pvid,
                            bid,
                            tenantId(),
                            baseDateTime,
                            toDate
                    );

            long deltaOnHand = 0;
            long deltaReserved = 0;

            for (StockTransaction txn : txns) {
                switch (txn.getType()) {

                    case RECEIPT, ADJUSTMENT, TRANSFER_IN, RETURN ->
                            deltaOnHand += txn.getQuantityDelta();

                    case SALE, TRANSFER_OUT ->
                            deltaOnHand += txn.getQuantityDelta();

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
            map.put("snapshotDate", baseDate);

            result.add(map);
        }

        return result;
    }

    @Transactional(readOnly = true)
    public boolean inventoryExists(UUID variantId, UUID branchId) {
        return inventoryItemRepository
                .findByProductVariantIdAndTenantIdAndBranchIdAndDeletedFalse(
                        variantId,
                        tenantId(),
                        branchId
                )
                .isPresent();
    }

    private void recomputeWeightedAverage(UUID variantId, UUID branchId) {

        InventoryItem item = getItemOrThrow(variantId, branchId);

        Object[] result = batchRepository.computeWeightedAverageRaw(
                variantId,
                tenantId(),
                branchId
        );

        BigDecimal totalValue = (BigDecimal) result[0];
        long totalQty = ((Number) result[1]).longValue();

        BigDecimal newAverage =
                totalQty > 0
                        ? totalValue.divide(BigDecimal.valueOf(totalQty), 6, RoundingMode.HALF_UP)
                        : BigDecimal.ZERO;

        item.setAverageCost(newAverage);
        inventoryItemRepository.save(item);
    }

    @Transactional(readOnly = true)
    public List<InventoryBatchDTO> getBatchesForVariantBranch(UUID variantId, UUID branchId) {

        Map<UUID, Long> reservedMap =
                stockEngine.reservedPerBatchPublic(variantId, branchId);

        List<InventoryBatch> batches =
                batchRepository.findAvailableBatches(
                        variantId,
                        tenantId(),
                        branchId
                );

        List<InventoryBatchDTO> result = new ArrayList<>();

        for (InventoryBatch batch : batches) {

            long reserved = reservedMap.getOrDefault(batch.getId(), 0L);
            long available = batch.getQuantityRemaining() - reserved;

            BigDecimal totalValue =
                    batch.getUnitCost()
                            .multiply(BigDecimal.valueOf(batch.getQuantityRemaining()));

            result.add(
                    InventoryBatchDTO.builder()
                            .batchId(batch.getId())
                            .productVariantId(batch.getProductVariantId())
                            .branchId(batch.getBranchId())
                            .unitCost(batch.getUnitCost())
//                            .unitSellingPrice(batch.getUnitSellingPrice())
                            .quantityReceived(batch.getQuantityReceived())
                            .quantityRemaining(batch.getQuantityRemaining())
                            .reservedQuantity(reserved)        // 🔥 NEW
                            .availableQuantity(available)      // 🔥 NEW
                            .totalRemainingValue(totalValue)
                            .receivedAt(batch.getReceivedAt())
                            .build()
            );
        }

        return result;
    }

    @Transactional(readOnly = true)
    public List<BatchConsumptionDTO> getBatchConsumptions(UUID batchId) {

        List<BatchConsumption> consumptions =
                batchConsumptionRepository.findByBatch_IdAndTenantIdAndBranchId(
                        batchId,
                        tenantId(),
                        branchId()
                );

        List<BatchConsumptionDTO> result = new ArrayList<>();

        for (BatchConsumption bc : consumptions) {

            BigDecimal total =
                    bc.getUnitCost()
                            .multiply(BigDecimal.valueOf(bc.getQuantity()));

            result.add(
                    BatchConsumptionDTO.builder()
                            .batchId(bc.getBatch().getId())
                            .saleId(bc.getSaleId())
                            .productVariantId(bc.getProductVariantId())
                            .quantity(bc.getQuantity())
                            .unitCost(bc.getUnitCost())
                            .totalCost(total)
                            .build()
            );
        }

        return result;
    }

    private Map<String, Long> computeReservedBulkMultiBranch(List<UUID> variantIds) {

        if (variantIds == null || variantIds.isEmpty()) {
            return Collections.emptyMap();
        }

        List<VariantReservationSummary> rows =
                batchReservationRepository.sumReservedBulk(
                        variantIds,
                        tenantId()
                );

        Map<String, Long> map = new HashMap<>();

        for (VariantReservationSummary row : rows) {

            String key = buildKey(
                    row.getProductVariantId(),
                    row.getBranchId()
            );

            map.put(key, row.getTotalReserved());
        }

        return map;
    }

    @Transactional
    public ApiResponse reconcileInventory(UUID variantId, UUID branchId) {

        InventoryItem item = getItemOrThrow(variantId, branchId);

        long batchSum = batchRepository.sumRemainingByVariant(
                variantId,
                tenantId(),
                branchId
        );
        boolean consistent = item.getQuantityOnHand() == batchSum;

        long reserved = stockEngine.reservedQuantity(variantId, branchId);

        Map<String, Object> result = new HashMap<>();
        result.put("quantityOnHand", item.getQuantityOnHand());
        result.put("batchSum", batchSum);
        result.put("reserved", reserved);
        result.put("available", batchSum - reserved);


        result.put("status",
                item.getQuantityOnHand() == batchSum ? "CONSISTENT" : "MISMATCH");

        return new ApiResponse("success", "Reconciliation result", result);
    }

    @Transactional(readOnly = true)
    public long getReservedQuantity(UUID variantId, UUID branchId) {
        return stockEngine.reservedQuantity(variantId, branchId);

    }

    public long availableQuantity(UUID variantId, UUID branchId) {
        return stockEngine.availableQuantity(variantId, branchId);
    }

    public AllocationResult previewAllocation(
            UUID variantId,
            UUID branchId,
            long quantity,
            List<UUID> selectedBatchIds
    ) {
        return stockEngine.previewAllocation(variantId, branchId, quantity, selectedBatchIds);
    }

    @Transactional
    public void reserveStockVariant(
            UUID variantId,
            UUID packagingId,
            UUID branchId,
            long baseUnits,
            long requestedQuantity,
            String reference,
            List<SaleLineBatchSelection> selections
    ) {
        if (baseUnits <= 0) {
            throw new IllegalArgumentException("baseUnits must be > 0");
        }

        if (requestedQuantity <= 0) {
            throw new IllegalArgumentException("quantity must be > 0");
        }

        UUID refId = requireReferenceId(reference, "Reservation");

        stockEngine.reserveWithSelection(
                refId,
                variantId,
                packagingId,
                branchId,
                baseUnits,
                requestedQuantity,
                selections
        );
    }

    @Transactional
    public void releaseReservationVariant(
            UUID branchId,
            String reference
    ) {
        UUID saleId = requireReferenceId(reference, "Release");
        stockEngine.release(branchId, saleId);
    }

    @Transactional
    public void decrementVariantStock(
            UUID variantId,
            UUID branchId,
            long quantity,
            String reference
    ) {
        if (!reference.startsWith("SALE:")) {
            throw new IllegalStateException("Consumption must originate from Sellable pipeline");
        }

        UUID saleId = requireReferenceId(reference, "Sale");

        stockEngine.consumeAndSync(variantId, branchId, saleId);
    }

    public List<InventoryBatch> suggestBatches(
            UUID variantId,
            UUID branchId,
            long quantity
    ) {
        return stockEngine.allocate(variantId, branchId, quantity);
    }

    // ------------------------
    // Helpers
    // ------------------------


    private InventoryResponse buildResponse(InventoryItem item) {
        long reserved = stockEngine.reservedQuantity(item.getProductVariant().getId(), item.getBranchId());
        return buildResponse(item, reserved);
    }

    private InventoryResponse buildResponse(InventoryItem item, long reserved) {

        UUID variantId = item.getProductVariant().getId();
        UUID branchId = item.getBranchId();

        Object[] stats = batchRepository.aggregateBatchStats(variantId, tenantId(), branchId);

        int batchCount = ((Number) stats[0]).intValue();
        BigDecimal totalValue = (BigDecimal) stats[1];
        LocalDateTime oldest = (LocalDateTime) stats[2];

        long available = stockEngine.availableQuantity(variantId, branchId);

        return InventoryResponse.builder()
                .productId(item.getProductVariant().getProduct().getId())
                .productName(item.getProductVariant().getProduct().getName())
                .productSKU(item.getProductVariant().getProduct().getSku())
                .productVariantId(variantId)
                .productClassification(item.getProductVariant().getClassification())
                .productVariantSKU(item.getProductVariant().getSku())
                .branchId(branchId)
                .averageCost(item.getAverageCost())
                .quantityOnHand(item.getQuantityOnHand())
                .quantityReserved(reserved)
                .quantityAvailable(available)
                .batchCount(batchCount)
                .oldestBatchDate(oldest != null ? oldest.toString() : null)
                .totalRemainingBatchValue(totalValue)
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

    private InventoryItem getItemOrThrow(UUID variantId, UUID branchId) {
        return inventoryItemRepository
                .findByProductVariantIdAndTenantIdAndBranchIdAndDeletedFalse(
                        variantId,
                        tenantId(),
                        branchId
                )
                .orElseThrow(() ->
                        new IllegalArgumentException("Inventory item not found"));
    }

    private <T> T assertNotDuplicateOrIgnore(java.util.function.Supplier<T> operation,
                                             String reference,
                                             StockTransaction.TransactionType type,
                                             UUID branchId) {

        try {
            return operation.get();

        } catch (org.springframework.dao.DataIntegrityViolationException ex) {

            if (reference != null) {

                boolean exists =
                        stockTransactionRepository.existsByReferenceAndTypeAndTenantIdAndBranchId(
                                reference,
                                type,
                                tenantId(),
                                branchId
                        );

                if (exists) {
                    return (T) new ApiResponse("success", "Idempotent replay", null);
                }
            }

            throw ex;
        }
    }

    private void validateInventoryInvariant(UUID variantId, UUID branchId) {

        InventoryItem item = getItemOrThrow(variantId, branchId);

        long batchSum = batchRepository.sumRemainingByVariant(
                variantId,
                tenantId(),
                branchId
        );

        if (item.getQuantityOnHand() != batchSum) {
            throw new IllegalStateException(
                    "Inventory invariant violated: item=" + item.getQuantityOnHand()
                            + " batchSum=" + batchSum
            );
        }
    }

    private UUID extractReferenceId(String reference) {

        if (reference == null) return null;

        if (!reference.contains(":")) {
            return null;
        }

        try {
            String[] parts = reference.split(":");
            return UUID.fromString(parts[1]);
        } catch (Exception e) {
            return null;
        }
    }

    private UUID requireReferenceId(String reference, String context) {
        UUID id = extractReferenceId(reference);

        if (id == null) {
            throw new IllegalArgumentException(
                    context + " requires reference in format TYPE:UUID"
            );
        }

        return id;
    }

    private String buildKey(UUID variantId, UUID branchId) {
        return variantId + "|" + branchId;
    }

    @Transactional(readOnly = true)
    public BigDecimal getAverageCost(UUID variantId, UUID branchId) {

        return inventoryItemRepository
                .findByProductVariantIdAndTenantIdAndBranchIdAndDeletedFalse(
                        variantId,
                        tenantId(),
                        branchId
                )
                .map(InventoryItem::getAverageCost)
                .orElse(BigDecimal.ZERO);
    }
}