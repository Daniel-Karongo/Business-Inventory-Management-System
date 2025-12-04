package com.IntegrityTechnologies.business_manager.modules.stock.inventory.service;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import com.IntegrityTechnologies.business_manager.exception.OutOfStockException;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.repository.SupplierRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.stock.category.repository.CategoryRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.AdjustStockRequest;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.InventoryResponse;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.ReceiveStockRequest;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.SupplierUnit;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryItem;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventorySnapshot;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.StockTransaction;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryItemRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventorySnapshotRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.StockTransactionRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductAudit;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service.ProductService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.service.ProductVariantService;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import jakarta.persistence.OptimisticLockException;
import lombok.RequiredArgsConstructor;
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

    private static final int MAX_RETRIES = 5;
    private final ProductVariantService productVariantService;

    // ------------------------
    // EXISTING / CORE METHODS
    // ------------------------

    @Transactional
    public ApiResponse receiveStock(ReceiveStockRequest req) {

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

                // IMPORTANT: MUST SAVE NOW TO AVOID TRANSIENT ERRORS
                variant = productVariantRepository.save(variant);
            }
        }

        // Ensure variant is saved even if previously found but unsaved
        if (variant.getId() == null) {
            variant = productVariantRepository.save(variant);
        }


        // -------------------------------------------------------------
        // 3. COMPUTE INCOMING COST + UNITS
        // -------------------------------------------------------------
        long incomingUnits = 0;
        BigDecimal incomingCostTotal = BigDecimal.ZERO;

        Set<Supplier> suppliersUsed = new HashSet<>();

        for (SupplierUnit su : req.getSuppliers()) {

            if (su.getUnitsSupplied() == null || su.getUnitsSupplied() <= 0)
                throw new IllegalArgumentException("unitsSupplied must be > 0");

            if (su.getUnitCost() == null)
                throw new IllegalArgumentException("unitCost must be provided");

            incomingUnits += su.getUnitsSupplied();
            incomingCostTotal = incomingCostTotal.add(
                    su.getUnitCost().multiply(BigDecimal.valueOf(su.getUnitsSupplied()))
            );

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
        // 6. UPDATE VARIANT PRICING (NEW RULE)
        //     newMinSell = newAvgCost × (1 + minimumPercentageProfit)
        // -------------------------------------------------------------
        double marginPercent = product.getMinimumPercentageProfit() == null
                ? 0
                : product.getMinimumPercentageProfit();

        BigDecimal marginMultiplier = BigDecimal.valueOf(1 + marginPercent);

        variant.setAverageBuyingPrice(newAvgCost);
        variant.setMinimumSellingPrice(
                productVariantService.computeMinSelling(newAvgCost, product.getMinimumPercentageProfit())
        );

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
        if (category != null)
            categoryRepository.save(category);

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
        // 9. RETURN RESPONSE
        // -------------------------------------------------------------
        return new ApiResponse("success", "Stock received successfully", buildResponse(item));
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

        InventoryItem item = inventoryItemRepository.findByProductVariant_IdAndBranchId(productVariantId, branchId)
                .orElseThrow(() -> new IllegalArgumentException("Inventory item not found"));

        long releaseQty = Math.min(quantity, item.getQuantityReserved());

        if (((item.getQuantityReserved() - releaseQty) < 0) || (item.getQuantityReserved() == 0)) {
            throw new IllegalArgumentException("You cannot release more than you have reserved, which is " + item.getQuantityReserved());
        }

        item.setQuantityReserved(item.getQuantityReserved() - releaseQty);
        item.setLastUpdatedAt(LocalDateTime.now());
        item.setLastUpdatedBy(getCurrentUsername());
        inventoryItemRepository.save(item);

        stockTransactionRepository.save(StockTransaction.builder()
                .productId(item.getProductVariant().getProduct().getId())
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
    public void decrementVariantStock(UUID productVariantId, UUID branchId, int quantity, String reference) {
        if (quantity <= 0) throw new IllegalArgumentException("Quantity must be > 0");

        int attempts = 0;
        while (true) {
            try {
                processStockDecrementVariant(productVariantId, branchId, quantity, reference);
                return;
            } catch (OptimisticLockException e) {
                attempts++;
                if (attempts >= MAX_RETRIES) {
                    throw new RuntimeException("Concurrent stock update conflict. Try again.");
                }
            }
        }
    }

    private void processStockDecrementVariant(UUID productVariantId, UUID branchId, long qty, String reference) {

        InventoryItem item = inventoryItemRepository.findByProductVariant_IdAndBranchId(productVariantId, branchId)
                .orElseThrow(() -> new OutOfStockException("Inventory record not found"));

        long available = item.getQuantityOnHand() - item.getQuantityReserved();
        if (available < qty) {
            throw new OutOfStockException("Insufficient stock in this branch");
        }

        item.setQuantityOnHand(item.getQuantityOnHand() - qty);
        item.setLastUpdatedAt(LocalDateTime.now());
        item.setLastUpdatedBy(getCurrentUsername());
        inventoryItemRepository.save(item);

        stockTransactionRepository.save(StockTransaction.builder()
                .productId(item.getProductVariant().getProduct().getId())
                .productVariantId(productVariantId)
                .branchId(branchId)
                .type(StockTransaction.TransactionType.SALE)
                .quantityDelta(-qty)
                .reference(reference)
                .note("Sale decrement")
                .timestamp(LocalDateTime.now())
                .performedBy(getCurrentUsername())
                .build()
        );
    }

    // ------------------------
    // NEW: variant-aware adjustStockVariant
    // ------------------------
    @Transactional
    public ApiResponse adjustStockVariant(AdjustStockRequest req) {
        if (req.getProductVariantId() == null) {
            throw new IllegalArgumentException("productVariantId is required");
        }
        if (req.getBranchId() == null) {
            throw new IllegalArgumentException("branchId is required");
        }

        ProductVariant variant = productVariantRepository.findById(req.getProductVariantId())
                .orElseThrow(() -> new IllegalArgumentException("ProductVariant not found"));
        Product product = variant.getProduct();
        if (product == null) throw new IllegalArgumentException("Variant's product not found");

        Branch branch = branchRepository.findById(req.getBranchId())
                .orElseThrow(() -> new IllegalArgumentException("Branch not found"));

        InventoryItem item = inventoryItemRepository.findByProductVariant_IdAndBranchId(req.getProductVariantId(), req.getBranchId())
                .orElseThrow(() -> new IllegalArgumentException("Inventory item not found for variant/branch"));

        long delta = req.getQuantityDelta();
        long newQty = Math.max(0L, item.getQuantityOnHand() + delta);
        item.setQuantityOnHand(newQty);
        item.setLastUpdatedAt(LocalDateTime.now());
        item.setLastUpdatedBy(getCurrentUsername());
        inventoryItemRepository.save(item);

        StockTransaction txn = StockTransaction.builder()
                .productId(product.getId())
                .productVariantId(variant.getId())
                .branchId(branch.getId())
                .type(StockTransaction.TransactionType.ADJUSTMENT)
                .quantityDelta(delta)
                .note(req.getReason())
                .reference(req.getReference())
                .timestamp(LocalDateTime.now())
                .performedBy(getCurrentUsername())
                .build();
        stockTransactionRepository.save(txn);

        return new ApiResponse("success", "Stock adjusted", buildResponse(item));
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
                            deltaOnHand -= txn.getQuantityDelta();

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
}