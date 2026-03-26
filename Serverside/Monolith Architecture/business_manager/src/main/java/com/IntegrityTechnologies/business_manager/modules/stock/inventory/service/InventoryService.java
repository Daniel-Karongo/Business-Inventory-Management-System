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
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto.AllocationDetail;
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
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.*;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.*;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.projection.BatchReservationSummary;
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
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.service.ProductVariantService;
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
import java.util.stream.Collectors;

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
            // 4. LOAD OR CREATE INVENTORY ITEM (🔥 FIXED WITH LOCK)
            // -------------------------------------------------------------

            final UUID variantIdFinal = variant.getId();
            final UUID branchIdFinal = branch.getId();
            final UUID productIdFinal = product.getId();
            final UUID tenantIdFinal = tenantId();

            InventoryItem item = inventoryItemRepository
                    .lockByVariant(
                            variantIdFinal,
                            tenantIdFinal,
                            branchIdFinal
                    )
                    .orElseGet(() -> {

                        InventoryItem newItem = InventoryItem.builder()
                                .productId(productIdFinal)
                                .productVariantId(variantIdFinal)
                                .branchId(branchIdFinal)
                                .quantityOnHand(0L)
                                .averageCost(BigDecimal.ZERO)
                                .deleted(false)
                                .build();

                        return inventoryItemRepository.save(newItem);
                    });

            long oldQty = item.getQuantityOnHand();

            item.setQuantityOnHand(oldQty + incomingUnits);
            item.setLastUpdatedAt(LocalDateTime.now());
            item.setLastUpdatedBy(getCurrentUsername());

            inventoryItemRepository.save(item);

            // -------------------------------------------------------------
            // 5. FIFO BATCH CREATION (UNCHANGED)
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

                UUID receiptId = requireReferenceId(req.getReference(), "Receipt");

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
                    buildResponse(item)
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

            long reserved = computeReserved(variantId, fromBranch);
            long available = sourceItem.getQuantityOnHand() - reserved;

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
            // 🔒 3. LOCK SOURCE FIFO BATCHES
            // ======================================================
            List<InventoryBatch> sourceBatches =
                    batchRepository.lockBatchesByBranch(
                            variantId,
                            tenantId,
                            fromBranch
                    );

            long remaining = quantity;
            BigDecimal totalTransferValue = BigDecimal.ZERO;

            // ======================================================
            // 4️⃣ FIFO MOVE (UNCHANGED LOGIC, NOW LOCKED)
            // ======================================================
            Map<UUID, Long> reservedMap = computeReservedPerBatch(variantId, fromBranch);

            for (InventoryBatch sourceBatch : sourceBatches) {

                if (remaining <= 0) break;

                reserved = reservedMap.getOrDefault(sourceBatch.getId(), 0L);
                available = sourceBatch.getQuantityRemaining() - reserved;

                long moveQty = Math.min(available, remaining);

                if (moveQty <= 0) continue;

                sourceBatch.setQuantityRemaining(
                        sourceBatch.getQuantityRemaining() - moveQty
                );

                BigDecimal cost =
                        sourceBatch.getUnitCost()
                                .multiply(BigDecimal.valueOf(moveQty));

                totalTransferValue = totalTransferValue.add(cost);

                // DESTINATION COST STRATEGY
                BigDecimal destinationCost =
                        req.getDestinationUnitCost() != null
                                ? req.getDestinationUnitCost()
                                : sourceBatch.getUnitCost();

                InventoryBatch newBatch = InventoryBatch.builder()
                        .productVariantId(variantId)
                        .branchId(toBranch)
                        .unitCost(destinationCost)
//                        .unitSellingPrice(sourceBatch.getUnitSellingPrice())
                        .quantityReceived(moveQty)
                        .quantityRemaining(moveQty)
                        .receivedAt(sourceBatch.getReceivedAt())
                        .build();

                batchRepository.save(newBatch);

                remaining -= moveQty;
            }

            if (remaining > 0)
                throw new OutOfStockException("Insufficient stock for transfer");

            // ======================================================
            // 5️⃣ UPDATE INVENTORY ITEMS (LOCKED)
            // ======================================================
            sourceItem.setQuantityOnHand(sourceItem.getQuantityOnHand() - quantity);
            sourceItem.setLastUpdatedAt(LocalDateTime.now());
            sourceItem.setLastUpdatedBy(getCurrentUsername());

            destinationItem.setQuantityOnHand(destinationItem.getQuantityOnHand() + quantity);
            destinationItem.setLastUpdatedAt(LocalDateTime.now());
            destinationItem.setLastUpdatedBy(getCurrentUsername());

            inventoryItemRepository.save(sourceItem);
            inventoryItemRepository.save(destinationItem);

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

    private void decrementPhysicalOnly(UUID variantId, UUID branchId, long qty) {

        InventoryItem item = getItemOrThrow(variantId, branchId);

        item.setQuantityOnHand(item.getQuantityOnHand() - qty);
        inventoryItemRepository.save(item);
    }

    private void incrementPhysicalOnly(UUID variantId, UUID branchId, long qty) {

        InventoryItem item = getItemOrThrow(variantId, branchId);

        item.setQuantityOnHand(item.getQuantityOnHand() + qty);
        inventoryItemRepository.save(item);
    }

    @Transactional
    public void restoreConsumedBatches(UUID saleId, UUID branchId, UUID variantId) {

        List<BatchConsumption> consumptions =
                batchConsumptionRepository.findBySaleIdAndProductVariantIdAndTenantIdAndBranchId(
                        saleId,
                        variantId,
                        tenantId(),
                        branchId
                );

        for (BatchConsumption bc : consumptions) {

            InventoryBatch batch = batchRepository.findByIdAndTenantIdAndBranchId(bc.getBatch().getId(), tenantId(), branchId)
                    .orElseThrow();

            batch.setQuantityRemaining(
                    batch.getQuantityRemaining() + bc.getQuantity()
            );
        }

        long totalRestore = consumptions.stream()
                .mapToLong(BatchConsumption::getQuantity)
                .sum();

        incrementPhysicalOnly(variantId, branchId, totalRestore);
        recomputeWeightedAverage(variantId, branchId);
        validateInventoryInvariant(variantId, branchId);
    }

    // ------------------------
    // VARIANT RESERVE / RELEASE / DECREMENT (existing)
    // ------------------------
    public void reserveStockVariant(UUID productVariantId,
                                    UUID branchId,
                                    Long quantity,
                                    String reference,
                                    List<SaleLineBatchSelection> manualSelections) {

        int attempts = 0;

        while (true) {
            try {
                doReserveStockVariant(
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
                    throw new RuntimeException("Failed to reserve stock due to concurrent updates.", e);
                }
            }
        }
    }

    @Transactional
    public void doReserveStockVariant(UUID productVariantId,
                                         UUID branchId,
                                         Long quantity,
                                         String reference,
                                         List<SaleLineBatchSelection> manualSelections) {

        processReservationVariant(
                productVariantId,
                branchId,
                quantity,
                reference,
                manualSelections
        );
    }

    private void processReservationVariant(UUID productVariantId,
                                           UUID branchId,
                                           long quantity,
                                           String reference,
                                           List<SaleLineBatchSelection> manualSelections) {

        assertNotDuplicateOrIgnore(() -> {
            final UUID tenantId = tenantId();
            UUID referenceId = requireReferenceId(reference, "Reservation");

            InventoryItem item = inventoryItemRepository
                    .lockByVariant(productVariantId, tenantId, branchId)
                    .orElseThrow(() -> new OutOfStockException("Inventory not found"));

            Map<UUID, Long> reservedMap = computeReservedPerBatch(productVariantId, branchId);

            long remaining = quantity;

            // =====================================================
            // 🔥 STRICT MODE (MANUAL)
            // =====================================================
            if (manualSelections != null && !manualSelections.isEmpty()) {

                List<UUID> orderedBatchIds = manualSelections.stream()
                        .map(SaleLineBatchSelection::getBatchId)
                        .sorted(Comparator.comparing(UUID::toString))
                        .toList();

                Map<UUID, SaleLineBatchSelection> map =
                        manualSelections.stream()
                                .collect(Collectors.toMap(SaleLineBatchSelection::getBatchId, s -> s));

                for (UUID batchId : orderedBatchIds) {

                    SaleLineBatchSelection sel = map.get(batchId);

                    InventoryBatch batch = batchRepository
                            .findByIdForUpdate(batchId, tenantId, branchId)
                            .orElseThrow(() -> new IllegalArgumentException("Invalid batch"));

                    long alreadyReserved = reservedMap.getOrDefault(batchId, 0L);
                    long available = batch.getQuantityRemaining() - alreadyReserved;

                    long reserveQty = sel.getQuantity();

                    if (available < reserveQty) {
                        throw new OutOfStockException("Insufficient stock in selected batch");
                    }

                    batchReservationRepository.save(
                            BatchReservation.builder()
                                    .batch(batch)
                                    .productVariantId(productVariantId)
                                    .referenceId(referenceId)
                                    .quantity(reserveQty)
                                    .createdAt(LocalDateTime.now())
                                    .build()
                    );

                    remaining -= reserveQty;
                }

                if (remaining != 0) {
                    throw new IllegalStateException("STRICT mode violation");
                }
            }

            // =====================================================
            // 🔥 FIFO MODE
            // =====================================================
            else {

                List<InventoryBatch> batches =
                        batchRepository.lockAvailableBatchesFIFO(
                                productVariantId,
                                tenantId,
                                branchId
                        );

                for (InventoryBatch batch : batches) {

                    if (remaining <= 0) break;

                    long alreadyReserved = reservedMap.getOrDefault(batch.getId(), 0L);
                    long available = batch.getQuantityRemaining() - alreadyReserved;

                    if (available <= 0) continue;

                    long reserveQty = Math.min(available, remaining);

                    batchReservationRepository.save(
                            BatchReservation.builder()
                                    .batch(batch)
                                    .productVariantId(productVariantId)
                                    .referenceId(referenceId)
                                    .quantity(reserveQty)
                                    .createdAt(LocalDateTime.now())
                                    .build()
                    );

                    remaining -= reserveQty;
                }

                if (remaining > 0) {
                    throw new OutOfStockException("Insufficient stock to reserve");
                }
            }

            // =====================================================
            // 🔥 TRANSACTION LOG
            // =====================================================
            stockTransactionRepository.save(
                    StockTransaction.builder()
                            .productVariantId(productVariantId)
                            .branchId(branchId)
                            .type(StockTransaction.TransactionType.RESERVATION)
                            .quantityDelta(quantity)
                            .reference(reference)
                            .note("Logical reservation (no batch mutation)")
                            .timestamp(LocalDateTime.now())
                            .performedBy(getCurrentUsername())
                            .build()
            );

            return new ApiResponse("success", "Idempotent replay", null);
        }, reference, StockTransaction.TransactionType.RESERVATION, branchId);
    }

    public void releaseReservationVariant(UUID productVariantId,
                                          UUID branchId,
                                          long quantity,
                                          String reference) {

        int attempts = 0;

        while (true) {
            try {
                doReleaseReservationVariant(
                        productVariantId,
                        branchId,
                        quantity,
                        reference
                );
                return;

            } catch (OptimisticLockException e) {
                if (++attempts >= MAX_RETRIES) {
                    throw new RuntimeException("Failed to release reservation", e);
                }
            }
        }
    }

    @Transactional
    public void doReleaseReservationVariant(UUID productVariantId,
                                            UUID branchId,
                                            long quantity,
                                            String reference) {

        processReleaseReservation(
                productVariantId,
                branchId,
                quantity,
                reference
        );
    }

    private void processReleaseReservation(UUID productVariantId,
                                           UUID branchId,
                                           long quantity,
                                           String reference) {

        assertNotDuplicateOrIgnore(() -> {
            final UUID tenantId = tenantId();

            UUID referenceId = requireReferenceId(reference, "Release");

            List<BatchReservation> reservations =
                    batchReservationRepository.lockByReferenceIdAndTenantIdAndBranchId(
                            referenceId,
                            tenantId,
                            branchId
                    );

            long remaining = quantity;

            for (BatchReservation res : reservations) {

                if (remaining <= 0) break;

                InventoryBatch batch = batchRepository.findByIdAndTenantIdAndBranchId(res.getBatch().getId(), tenantId, branchId)
                        .orElseThrow();

                long releaseQty = Math.min(res.getQuantity(), remaining);

                res.setQuantity(res.getQuantity() - releaseQty);

                if (res.getQuantity() == 0) {
                    batchReservationRepository.delete(res);
                }

                remaining -= releaseQty;
            }

            if (remaining > 0) {
                throw new IllegalStateException("Attempted to release more than reserved");
            }

            stockTransactionRepository.save(
                    StockTransaction.builder()
                            .productVariantId(productVariantId)
                            .branchId(branchId)
                            .type(StockTransaction.TransactionType.RELEASE)
                            .quantityDelta(quantity)
                            .reference(reference)
                            .note("Batch release")
                            .timestamp(LocalDateTime.now())
                            .performedBy(getCurrentUsername())
                            .build()
            );

            return new ApiResponse("success", "Idempotent replay", null);
        }, reference, StockTransaction.TransactionType.RELEASE, branchId);
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
    public void decrementVariantStock(UUID productVariantId,
                                      UUID branchId,
                                      int quantity,
                                      String reference,
                                      List<SaleLineBatchSelection> manualSelections) {

        int attempts = 0;

        while (true) {
            try {
                doDecrementVariantStock(
                        productVariantId,
                        branchId,
                        quantity,
                        reference,
                        manualSelections
                );
                return;

            } catch (OptimisticLockException e) {
                if (++attempts >= MAX_RETRIES) {
                    throw new RuntimeException("Stock conflict", e);
                }
            }
        }
    }

    @Transactional
    public void doDecrementVariantStock(UUID productVariantId,
                                        UUID branchId,
                                        int quantity,
                                        String reference,
                                        List<SaleLineBatchSelection> manualSelections) {

        processStockDecrementVariant(
                productVariantId,
                branchId,
                quantity,
                reference,
                manualSelections
        );
    }

    private void processStockDecrementVariant(UUID productVariantId,
                                              UUID branchId,
                                              long qty,
                                              String reference,
                                              List<SaleLineBatchSelection> manualSelections) {

        assertNotDuplicateOrIgnore(() -> {
            periodGuardService.validateOpenPeriod(LocalDate.now(), branchId);

            final UUID tenantId = tenantId();
            UUID saleId = requireReferenceId(reference, "Sale");

            InventoryItem item = inventoryItemRepository
                    .lockByVariant(productVariantId, tenantId, branchId)
                    .orElseThrow(() -> new OutOfStockException("Inventory not found"));

            long remaining = qty;
            BigDecimal totalCost = BigDecimal.ZERO;

            // =====================================================
            // 🔥 1. STRICT MODE (MANUAL SELECTION)
            // =====================================================
            if (manualSelections != null && !manualSelections.isEmpty()) {

                // 🔥 deterministic ordering (deadlock-safe)
                List<UUID> orderedBatchIds = manualSelections.stream()
                        .map(SaleLineBatchSelection::getBatchId)
                        .sorted(Comparator.comparing(UUID::toString))
                        .toList();

                Map<UUID, SaleLineBatchSelection> map =
                        manualSelections.stream()
                                .collect(Collectors.toMap(
                                        SaleLineBatchSelection::getBatchId,
                                        s -> s
                                ));

                for (UUID batchId : orderedBatchIds) {

                    SaleLineBatchSelection sel = map.get(batchId);

                    InventoryBatch batch = batchRepository
                            .findByIdForUpdate(batchId, tenantId, branchId)
                            .orElseThrow(() -> new IllegalArgumentException("Invalid batch"));

                    long consumeQty = sel.getQuantity();

                    if (batch.getQuantityRemaining() < consumeQty) {
                        throw new OutOfStockException("Insufficient stock in selected batch");
                    }

                    batch.setQuantityRemaining(batch.getQuantityRemaining() - consumeQty);

                    totalCost = totalCost.add(
                            batch.getUnitCost().multiply(BigDecimal.valueOf(consumeQty))
                    );

                    batchConsumptionRepository.save(
                            BatchConsumption.builder()
                                    .batch(batch)
                                    .saleId(saleId)
                                    .productVariantId(productVariantId)
                                    .quantity(consumeQty)
                                    .unitCost(batch.getUnitCost())
                                    .build()
                    );

                    remaining -= consumeQty;
                }

                if (remaining != 0) {
                    throw new IllegalStateException("STRICT mode violation");
                }
            }

            // =====================================================
            // 🔥 2. RESERVATION FIRST
            // =====================================================
            else {

                List<BatchReservation> reservations =
                        batchReservationRepository.lockByReferenceIdAndTenantIdAndBranchId(
                                saleId,
                                tenantId,
                                branchId
                        );

                for (BatchReservation res : reservations) {

                    if (remaining <= 0) break;

                    InventoryBatch batch = batchRepository
                            .findByIdAndTenantIdAndBranchId(
                                    res.getBatch().getId(),
                                    tenantId,
                                    branchId
                            )
                            .orElseThrow(() -> new IllegalStateException("Batch not found"));

                    long consumeQty = Math.min(res.getQuantity(), remaining);

                    if (batch.getQuantityRemaining() < consumeQty) {
                        throw new IllegalStateException("Batch inconsistency detected");
                    }

                    batch.setQuantityRemaining(batch.getQuantityRemaining() - consumeQty);

                    totalCost = totalCost.add(
                            batch.getUnitCost().multiply(BigDecimal.valueOf(consumeQty))
                    );

                    batchConsumptionRepository.save(
                            BatchConsumption.builder()
                                    .batch(batch)
                                    .saleId(saleId)
                                    .productVariantId(productVariantId)
                                    .quantity(consumeQty)
                                    .unitCost(batch.getUnitCost())
                                    .build()
                    );

                    res.setQuantity(res.getQuantity() - consumeQty);

                    if (res.getQuantity() == 0) {
                        batchReservationRepository.delete(res);
                    }

                    remaining -= consumeQty;
                }

                // =====================================================
                // 🔥 3. FIFO FALLBACK (SAFE)
                // =====================================================
                if (remaining > 0) {

                    List<InventoryBatch> batches =
                            batchRepository.lockAvailableBatchesFIFO(
                                    productVariantId,
                                    tenantId,
                                    branchId
                            );

                    Map<UUID, Long> reservedMap = computeReservedPerBatch(productVariantId, branchId);

                    for (InventoryBatch batch : batches) {

                        if (remaining <= 0) break;

                        long reserved = reservedMap.getOrDefault(batch.getId(), 0L);
                        long available = batch.getQuantityRemaining() - reserved;

                        long consumeQty = Math.min(available, remaining);

                        if (consumeQty <= 0) continue;

                        batch.setQuantityRemaining(batch.getQuantityRemaining() - consumeQty);

                        totalCost = totalCost.add(
                                batch.getUnitCost().multiply(BigDecimal.valueOf(consumeQty))
                        );

                        batchConsumptionRepository.save(
                                BatchConsumption.builder()
                                        .batch(batch)
                                        .saleId(saleId)
                                        .productVariantId(productVariantId)
                                        .quantity(consumeQty)
                                        .unitCost(batch.getUnitCost())
                                        .build()
                        );

                        remaining -= consumeQty;
                    }
                }
            }

            if (remaining > 0) {
                throw new OutOfStockException("Insufficient stock for sale");
            }

            // =====================================================
            // 🔥 FINALIZE INVENTORY + AUDIT + ACCOUNTING
            // =====================================================
            item.setQuantityOnHand(item.getQuantityOnHand() - qty);
            item.setLastUpdatedAt(LocalDateTime.now());
            item.setLastUpdatedBy(getCurrentUsername());

            inventoryItemRepository.save(item);

            recomputeWeightedAverage(productVariantId, branchId);
            validateInventoryInvariant(productVariantId, branchId);

            inventoryAccountingPort.recordInventoryConsumption(
                    tenantId,
                    saleId,
                    branchId,
                    totalCost,
                    reference
            );

            stockTransactionRepository.save(
                    StockTransaction.builder()
                            .productId(item.getProductId())
                            .productVariantId(productVariantId)
                            .branchId(branchId)
                            .type(StockTransaction.TransactionType.SALE)
                            .quantityDelta(-qty)
                            .reference(reference)
                            .note("Sale (ordered-lock safe)")
                            .timestamp(LocalDateTime.now())
                            .performedBy(getCurrentUsername())
                            .build()
            );

            return new ApiResponse("success", "Idempotent replay", null);
        }, reference, StockTransaction.TransactionType.SALE, branchId);
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

            // ======================================================
            // 🔒 1. LOCK INVENTORY ITEM
            // ======================================================
            InventoryItem item = inventoryItemRepository
                    .lockByVariant(variant.getId(), tenantId, branch.getId())
                    .orElseThrow(() -> new IllegalArgumentException("Inventory item not found"));

            long delta = req.getQuantityDelta();

            if (delta == 0)
                throw new IllegalArgumentException("Quantity delta cannot be zero");

            // ======================================================
            // NEGATIVE ADJUSTMENT (LOSS / DAMAGE)
            // ======================================================
            if (delta < 0) {

                long qtyToRemove = Math.abs(delta);

                long reserved = computeReserved(variant.getId(), branch.getId());
                long available = item.getQuantityOnHand() - reserved;

                if (available < qtyToRemove)
                    throw new OutOfStockException("Insufficient stock for negative adjustment");

                long remaining = qtyToRemove;
                BigDecimal totalCost = BigDecimal.ZERO;

                // 🔒 LOCK FIFO BATCHES
                List<InventoryBatch> batches =
                        batchRepository.lockAvailableBatchesFIFO(
                                variant.getId(),
                                tenantId,
                                branch.getId()
                        );

                Map<UUID, Long> reservedMap = computeReservedPerBatch(variant.getId(), branch.getId());

                for (InventoryBatch batch : batches) {

                    if (remaining <= 0) break;

                    reserved = reservedMap.getOrDefault(batch.getId(), 0L);
                    available = batch.getQuantityRemaining() - reserved;

                    long deduct = Math.min(available, remaining);

                    if (deduct <= 0) continue;

                    batch.setQuantityRemaining(batch.getQuantityRemaining() - deduct);

                    BigDecimal cost =
                            batch.getUnitCost()
                                    .multiply(BigDecimal.valueOf(deduct));

                    totalCost = totalCost.add(cost);

                    remaining -= deduct;
                }

                if (remaining > 0)
                    throw new OutOfStockException("Insufficient FIFO stock");

                // update inventory
                item.setQuantityOnHand(item.getQuantityOnHand() - qtyToRemove);
                item.setLastUpdatedAt(LocalDateTime.now());
                item.setLastUpdatedBy(getCurrentUsername());

                inventoryItemRepository.save(item);

                // accounting
                UUID refId = requireReferenceId(req.getReference(), "Adjustment");

                inventoryAccountingPort.recordInventoryConsumption(
                        tenantId,
                        refId,
                        branch.getId(),
                        totalCost,
                        req.getReference()
                );
            }

            // ======================================================
            // POSITIVE ADJUSTMENT (FOUND STOCK)
            // ======================================================
            else {

                long qtyToAdd = delta;

                BigDecimal unitCost = req.getUnitCost();

                if (unitCost == null)
                    unitCost = BigDecimal.ZERO;

                InventoryBatch batch = InventoryBatch.builder()
                        .productVariantId(variant.getId())
                        .branchId(branch.getId())
                        .unitCost(unitCost)
//                        .unitSellingPrice(sellingPrice)
                        .quantityReceived(qtyToAdd)
                        .quantityRemaining(qtyToAdd)
                        .receivedAt(LocalDateTime.now())
                        .build();

                batchRepository.save(batch);

                item.setQuantityOnHand(item.getQuantityOnHand() + qtyToAdd);
                item.setLastUpdatedAt(LocalDateTime.now());
                item.setLastUpdatedBy(getCurrentUsername());

                inventoryItemRepository.save(item);

                BigDecimal totalValue =
                        unitCost.multiply(BigDecimal.valueOf(qtyToAdd));

                UUID refId = requireReferenceId(req.getReference(), "Adjustment");

                inventoryAccountingPort.recordInventoryReturn(
                        tenantId,
                        refId,
                        branch.getId(),
                        totalValue,
                        req.getReference()
                );
            }

            // ======================================================
            // 🔁 RECOMPUTE AVERAGE
            // ======================================================
            recomputeWeightedAverage(variant.getId(), branch.getId());
            validateInventoryInvariant(variant.getId(), branch.getId());

            // ======================================================
            // STOCK TRANSACTION
            // ======================================================
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

            return new ApiResponse("success", "Batch-aware stock adjusted", buildResponse(item));
        }, req.getReference(), StockTransaction.TransactionType.ADJUSTMENT, req.getBranchId());
    }

    public void incrementVariantStock(UUID variantId,
                                      UUID branchId,
                                      long qty,
                                      String reference) {

        int attempts = 0;

        while (true) {
            try {
                doIncrementVariantStock(
                        variantId,
                        branchId,
                        qty,
                        reference
                );
                return;

            } catch (OptimisticLockException e) {
                if (++attempts >= MAX_RETRIES) {
                    throw new RuntimeException("Increment failed", e);
                }
            }
        }
    }

    @Transactional
    public void doIncrementVariantStock(UUID variantId,
                                        UUID branchId,
                                        long qty,
                                        String reference) {

        processIncrementVariantStock(
                variantId,
                branchId,
                qty,
                reference
        );
    }

    private void processIncrementVariantStock(UUID variantId,
                                              UUID branchId,
                                              long qty,
                                              String reference) {

        assertNotDuplicateOrIgnore(() -> {

            InventoryItem item = getItemOrThrow(variantId, branchId);

            BigDecimal unitCost = item.getAverageCost();
            if (unitCost == null) unitCost = BigDecimal.ZERO;

            ProductVariant variant = item.getProductVariant();

            InventoryBatch batch = InventoryBatch.builder()
                    .productVariantId(variantId)
                    .branchId(branchId)
                    .unitCost(unitCost)
//                    .unitSellingPrice(sellingPrice)
                    .quantityReceived(qty)
                    .quantityRemaining(qty)
                    .receivedAt(LocalDateTime.now())
                    .build();

            batchRepository.save(batch);

            item.setQuantityOnHand(item.getQuantityOnHand() + qty);
            item.setLastUpdatedAt(LocalDateTime.now());
            item.setLastUpdatedBy(getCurrentUsername());
            inventoryItemRepository.save(item);

            recomputeWeightedAverage(variantId, branchId);
            validateInventoryInvariant(variantId, branchId);

            BigDecimal value =
                    unitCost.multiply(BigDecimal.valueOf(qty));

            UUID refId = requireReferenceId(reference, "Return");

            inventoryAccountingPort.recordInventoryReturn(
                    tenantId(),
                    refId,
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
                            .unitCost(unitCost)
                            .type(StockTransaction.TransactionType.RETURN)
                            .reference(reference)
                            .timestamp(LocalDateTime.now())
                            .performedBy(getCurrentUsername())
                            .build()
            );

            return new ApiResponse("success", "Idempotent replay", null);

        }, reference, StockTransaction.TransactionType.RETURN, branchId);
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

                long qty = item.getQuantityOnHand();
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

    @Transactional(readOnly = true)
    public long availableQuantity(UUID variantId, UUID branchId) {

        Map<UUID, Long> reservedMap = computeReservedPerBatch(variantId, branchId);

        List<InventoryBatch> batches =
                batchRepository.findAvailableBatches(
                        variantId,
                        tenantId(),
                        branchId
                );

        long total = 0;

        for (InventoryBatch batch : batches) {

            long reserved = reservedMap.getOrDefault(batch.getId(), 0L);
            long available = batch.getQuantityRemaining() - reserved;

            if (available > 0) {
                total += available;
            }
        }

        return total;
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

        Map<UUID, Long> reservedMap = computeReservedPerBatch(variantId, branchId);

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

    @Transactional(readOnly = true)
    public AllocationResult previewAllocation(
            UUID variantId,
            UUID branchId,
            long quantity,
            List<UUID> selectedBatchIds
    ) {
        branchTenantGuard.validate(branchId);

        Map<UUID, Long> reservedMap = computeReservedPerBatch(variantId, branchId);

        long remaining = quantity;
        BigDecimal totalCost = BigDecimal.ZERO;

        List<AllocationDetail> rows = new ArrayList<>();

        List<InventoryBatch> batches =
                batchRepository.findAvailableBatches(
                        variantId,
                        tenantId(),
                        branchId
                );

        // 🔥 MANUAL FIRST
        if (selectedBatchIds != null && !selectedBatchIds.isEmpty()) {

            Map<UUID, InventoryBatch> batchMap =
                    batches.stream().collect(Collectors.toMap(InventoryBatch::getId, b -> b));

            for (UUID id : selectedBatchIds) {

                InventoryBatch batch = batchMap.get(id);
                if (batch == null)
                    throw new IllegalArgumentException("Invalid batch selected");

                long reserved = reservedMap.getOrDefault(batch.getId(), 0L);
                long available = batch.getQuantityRemaining() - reserved;

                long allocate = Math.min(available, remaining);

                BigDecimal cost =
                        batch.getUnitCost().multiply(BigDecimal.valueOf(allocate));

                rows.add(AllocationDetail.builder()
                        .batchId(batch.getId())
                        .allocatedQuantity(allocate)
                        .availableQuantity(available)
                        .reservedQuantity(reserved)
                        .unitCost(batch.getUnitCost())
                        .totalCost(cost)
                        .receivedAt(batch.getReceivedAt())
                        .build());

                totalCost = totalCost.add(cost);
                remaining -= allocate;
            }
        }

        // 🔥 FIFO
        for (InventoryBatch batch : batches) {

            if (remaining <= 0) break;

            if (selectedBatchIds != null && selectedBatchIds.contains(batch.getId()))
                continue;

            long reserved = reservedMap.getOrDefault(batch.getId(), 0L);
            long available = batch.getQuantityRemaining() - reserved;

            long allocate = Math.min(available, remaining);

            BigDecimal cost =
                    batch.getUnitCost().multiply(BigDecimal.valueOf(allocate));

            rows.add(AllocationDetail.builder()
                    .batchId(batch.getId())
                    .allocatedQuantity(allocate)
                    .availableQuantity(available)
                    .reservedQuantity(reserved)
                    .unitCost(batch.getUnitCost())
                    .totalCost(cost)
                    .receivedAt(batch.getReceivedAt())
                    .build());

            totalCost = totalCost.add(cost);
            remaining -= allocate;
        }

        if (remaining > 0)
            throw new OutOfStockException("Insufficient stock for allocation");

        return AllocationResult.builder()
                .allocations(rows)
                .totalCost(totalCost)
                .build();
    }

    public Map<UUID, Long> computeReservedPerBatch(UUID variantId, UUID branchId) {

        List<BatchReservationSummary> rows =
                batchReservationRepository.sumReservedByBatch(
                        variantId,
                        tenantId(),
                        branchId
                );

        Map<UUID, Long> map = new HashMap<>();

        for (BatchReservationSummary row : rows) {
            map.put(row.getBatchId(), row.getTotalReserved());
        }

        return map;
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

        long reserved = computeReserved(variantId, branchId);

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
        return computeReserved(variantId, branchId);
    }

    // ------------------------
    // Helpers
    // ------------------------


    private InventoryResponse buildResponse(InventoryItem item) {
        long reserved = computeReserved(item.getProductVariant().getId(), item.getBranchId());
        return buildResponse(item, reserved);
    }

    private InventoryResponse buildResponse(InventoryItem item, long reserved) {

        UUID variantId = item.getProductVariant().getId();
        UUID branchId = item.getBranchId();

        Object[] stats = batchRepository.aggregateBatchStats(variantId, tenantId(), branchId);

        int batchCount = ((Number) stats[0]).intValue();
        BigDecimal totalValue = (BigDecimal) stats[1];
        LocalDateTime oldest = (LocalDateTime) stats[2];

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
                .quantityAvailable(item.getQuantityOnHand() - reserved)
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

    @Transactional(readOnly = true)
    public List<Map<String, Object>> suggestBatches(
            UUID variantId,
            UUID branchId,
            long quantityNeeded
    ) {
        branchTenantGuard.validate(branchId);

        List<Map<String, Object>> result = new ArrayList<>();

        long remaining = quantityNeeded;

        List<InventoryBatch> batches =
                batchRepository.findAvailableBatches(
                    variantId,
                    tenantId(),
                    branchId
                );

        boolean first = true;
        Map<UUID, Long> reservedMap = computeReservedPerBatch(variantId, branchId);

        for (InventoryBatch batch : batches) {

            long reserved = reservedMap.getOrDefault(batch.getId(), 0L);
            long available = batch.getQuantityRemaining() - reserved;

            long suggested = 0;

            if (remaining > 0 && available > 0) {
                suggested = Math.min(available, remaining);
                remaining -= suggested;
            }

            Map<String, Object> row = new HashMap<>();
            row.put("batchId", batch.getId());
            row.put("available", available);     // 🔥 FIXED
            row.put("reserved", reserved);       // 🔥 NEW
            row.put("suggested", suggested);
            row.put("unitCost", batch.getUnitCost());
//            row.put("sellingPrice", batch.getUnitSellingPrice());
            row.put("receivedAt", batch.getReceivedAt());
            row.put("isFifo", first);

            result.add(row);

            first = false;
        }

        return result;
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

    private long computeReserved(UUID variantId, UUID branchId) {

        return batchReservationRepository
                .sumReservedByVariantAndTenantAndBranch(
                        variantId,
                        tenantId(),
                        branchId
                );
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