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
import com.IntegrityTechnologies.business_manager.modules.stock.product.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.repository.ProductAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.repository.ProductRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.repository.ProductVariantRepository;
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

    private static final int MAX_RETRIES = 5;

    // ------------------------
    // EXISTING / CORE METHODS
    // ------------------------

    @Transactional
    public ApiResponse receiveStock(ReceiveStockRequest req) {
        ProductVariant variant;

        if (req.getProductVariantId() != null) {
            // Use provided variant
            variant = productVariantRepository.findById(req.getProductVariantId())
                    .orElseThrow(() -> new IllegalArgumentException("ProductVariant not found"));
        } else {
            // Variant not provided -> require classification
            if (req.getClassification() == null || req.getClassification().isBlank()) {
                throw new IllegalArgumentException("Either productVariantId or classification must be provided");
            }

            // Does this classification exist already?
            variant = productVariantRepository
                    .findByProductIdAndClassification(req.getProductId(), req.getClassification())
                    .orElse(null);

            if (variant == null) {
                // Auto-create new variant
                Product product = productRepository.findById(req.getProductId())
                        .orElseThrow(() -> new IllegalArgumentException("Product not found"));

                variant = new ProductVariant();
                variant.setProduct(product);
                variant.setClassification(req.getClassification());
                variant.setDefaultSellingPrice(product.getPrice()); // default
                variant.setSku(req.getNewVariantSku() != null ? req.getNewVariantSku() : generateVariantSku(product));

                variant = productVariantRepository.save(variant);
            }
        }

        Product product = variant.getProduct();
        if (product == null) throw new IllegalArgumentException("Variant's product not found");

        Branch branch = branchRepository.findById(req.getBranchId())
                .orElseThrow(() -> new IllegalArgumentException("Branch not found"));

        InventoryItem item = inventoryItemRepository.findByProductVariantIdAndBranchId(req.getProductVariantId(), req.getBranchId())
                .orElse(InventoryItem.builder()
                        .productVariant(variant)
                        .branch(branch)
                        .quantityOnHand(0L)
                        .quantityReserved(0L)
                        .averageCost(BigDecimal.ZERO)
                        .build()
                );

        if (req.getSuppliers() == null || req.getSuppliers().isEmpty()) {
            throw new IllegalArgumentException("At least one supplier must be provided");
        }

        long quantityStocked = 0L;
        BigDecimal totalReceivedValue = BigDecimal.ZERO;
        Set<Supplier> productSuppliers = new HashSet<>();

        for (SupplierUnit supplierUnit : req.getSuppliers()) {

            if (supplierUnit.getUnitsSupplied() == null || supplierUnit.getUnitsSupplied() <= 0) {
                throw new IllegalArgumentException("unitsSupplied must be > 0");
            }

            if (supplierUnit.getUnitCost() == null) {
                throw new IllegalArgumentException("unitCost must be provided for supplier " + supplierUnit.getSupplierId());
            }

            long units = supplierUnit.getUnitsSupplied();
            BigDecimal unitCost = supplierUnit.getUnitCost();

            StockTransaction txn = StockTransaction.builder()
                    .productId(product.getId())
                    .productVariantId(variant.getId())
                    .branchId(req.getBranchId())
                    .type(StockTransaction.TransactionType.RECEIPT)
                    .quantityDelta(units)
                    .unitCost(unitCost)
                    .reference(req.getReference())
                    .supplierId(supplierUnit.getSupplierId())
                    .note(req.getNote())
                    .timestamp(LocalDateTime.now())
                    .performedBy(getCurrentUsername())
                    .build();

            stockTransactionRepository.save(txn);

            quantityStocked += units;
            totalReceivedValue = totalReceivedValue.add(unitCost.multiply(BigDecimal.valueOf(units)));

            supplierRepository.findByIdAndDeletedFalse(supplierUnit.getSupplierId())
                    .ifPresent(productSuppliers::add);
        }

        // update supplier links/audit
        Set<Supplier> addedProductSuppliers = new HashSet<>();
        Category productCategory = product.getCategory();
        productSuppliers.forEach(supplier -> {
            if (!product.getSuppliers().contains(supplier)) {
                product.getSuppliers().add(supplier);
                addedProductSuppliers.add(supplier);
            }
            if (productCategory != null && !productCategory.getSuppliers().contains(supplier)) {
                productCategory.getSuppliers().add(supplier);
            }
        });
        productRepository.save(product);
        createProductAudit(product, "UPDATE", "suppliers", null, addedProductSuppliers.stream().map(s -> s.getId().toString() + " - " + s.getName()).toString());
        if (productCategory != null) categoryRepository.save(productCategory);

        // update inventory item quantities & average cost (weighted avg on receive)
        long oldQty = item.getQuantityOnHand() != null ? item.getQuantityOnHand() : 0L;
        BigDecimal oldAvgCost = item.getAverageCost() != null ? item.getAverageCost() : BigDecimal.ZERO;

        long newQty = oldQty + quantityStocked;
        item.setQuantityOnHand(newQty);
        item.setLastUpdatedAt(LocalDateTime.now());
        item.setLastUpdatedBy(getCurrentUsername());

        if (quantityStocked > 0) {
            BigDecimal totalOldValue = oldAvgCost.multiply(BigDecimal.valueOf(oldQty));
            BigDecimal totalNewValue = totalOldValue.add(totalReceivedValue);

            BigDecimal newAvg = BigDecimal.ZERO;
            if (newQty > 0) {
                newAvg = totalNewValue.divide(BigDecimal.valueOf(newQty), 6, RoundingMode.HALF_UP);
            }
            item.setAverageCost(newAvg);
        }

        inventoryItemRepository.save(item);

        BigDecimal lastUnitCost = req.getSuppliers().get(req.getSuppliers().size() - 1).getUnitCost();
        product.setBuyingPrice(lastUnitCost);
        productRepository.save(product);

        return new ApiResponse("success", "Stock received", buildResponse(item));
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
        InventoryItem item = inventoryItemRepository.findByProductVariantIdAndBranchId(productVariantId, branchId)
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

        InventoryItem item = inventoryItemRepository.findByProductVariantIdAndBranchId(productVariantId, branchId)
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

        InventoryItem item = inventoryItemRepository.findByProductVariantIdAndBranchId(productVariantId, branchId)
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

        InventoryItem item = inventoryItemRepository.findByProductVariantIdAndBranchId(req.getProductVariantId(), req.getBranchId())
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
            Optional<InventoryItem> itemOpt = inventoryItemRepository.findByProductVariantIdAndBranchId(productVariantId, branchId);
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

            List<Map<String, Object>> aggregated = items.stream().map(it -> {
                Map<String, Object> m = new HashMap<>();
                m.put("variantId", it.getProductVariant().getId());
                m.put("productId", it.getProductVariant().getProduct().getId());
                m.put("variantClassification", it.getProductVariant().getClassification());
                m.put("branchId", it.getBranch().getId());
                m.put("branchName", it.getBranch().getName());
                m.put("quantityOnHand", it.getQuantityOnHand());
                m.put("quantityReserved", it.getQuantityReserved());
                m.put("averageCost", it.getAverageCost());
                return m;
            }).toList();

            return new ApiResponse("success", "Inventory for variant across branches", aggregated);
        }
    }

    /**
     * Return inventory for all variants of a product across branches.
     */
    public ApiResponse getInventoryForProduct(UUID productId) {
        if (productId == null) throw new IllegalArgumentException("productId is required");

        List<InventoryItem> items = inventoryItemRepository.findAll()
                .stream()
                .filter(it -> it.getProductVariant() != null &&
                        it.getProductVariant().getProduct() != null &&
                        productId.equals(it.getProductVariant().getProduct().getId()))
                .toList();

        List<Map<String, Object>> out = items.stream().map(it -> {
            Map<String, Object> m = new HashMap<>();
            m.put("productId", it.getProductVariant().getProduct().getId());
            m.put("productName", it.getProductVariant().getProduct().getName());
            m.put("variantId", it.getProductVariant().getId());
            m.put("variantClassification", it.getProductVariant().getClassification());
            m.put("branchId", it.getBranch().getId());
            m.put("branchName", it.getBranch().getName());
            m.put("quantityOnHand", it.getQuantityOnHand());
            m.put("quantityReserved", it.getQuantityReserved());
            m.put("averageCost", it.getAverageCost());
            return m;
        }).toList();

        return new ApiResponse("success", "Inventory for product (variants across branches)", out);
    }

    /**
     * Return stock across branches aggregated by variant for a product.
     */
    public List<Map<String, Object>> getStockAcrossBranches(UUID productId) {
        List<InventoryItem> items = inventoryItemRepository.findAll()
                .stream()
                .filter(it -> it.getProductVariant() != null &&
                        it.getProductVariant().getProduct() != null &&
                        productId.equals(it.getProductVariant().getProduct().getId()))
                .toList();

        List<Map<String, Object>> res = new ArrayList<>();
        for (InventoryItem it : items) {
            Map<String, Object> m = new HashMap<>();
            m.put("productId", it.getProductVariant().getProduct().getId());
            m.put("variantId", it.getProductVariant().getId());
            m.put("variantClassification", it.getProductVariant().getClassification());
            m.put("branchId", it.getBranch().getId());
            m.put("branchName", it.getBranch().getName());
            m.put("quantityOnHand", it.getQuantityOnHand());
            m.put("quantityReserved", it.getQuantityReserved());
            res.add(m);
        }
        return res;
    }

    public List<Map<String, Object>> getLowStock(Long threshold) {
        var items = inventoryItemRepository.findAll();
        return items.stream()
                .filter(i -> (i.getQuantityOnHand() - i.getQuantityReserved()) <= threshold)
                .map(i -> {
                    Map<String, Object> m = new HashMap<>();
                    m.put("productId", i.getProductVariant().getProduct().getId());
                    m.put("productName", i.getProductVariant().getProduct().getName());
                    m.put("variantId", i.getProductVariant().getId());
                    m.put("variantClassification", i.getProductVariant().getClassification());
                    m.put("branchId", i.getBranch().getId());
                    m.put("quantityOnHand", i.getQuantityOnHand());
                    m.put("quantityReserved", i.getQuantityReserved());
                    m.put("averageCost", i.getAverageCost());
                    return m;
                }).toList();
    }

    public List<Map<String, Object>> getOutOfStock() {
        var items = inventoryItemRepository.findAll();
        return items.stream()
                .filter(i -> (i.getQuantityOnHand() - i.getQuantityReserved()) <= 0)
                .map(i -> {
                    Map<String, Object> m = new HashMap<>();
                    m.put("productId", i.getProductVariant().getProduct().getId());
                    m.put("productName", i.getProductVariant().getProduct().getName());
                    m.put("variantId", i.getProductVariant().getId());
                    m.put("branchId", i.getBranch().getId());
                    m.put("quantityOnHand", i.getQuantityOnHand());
                    return m;
                }).toList();
    }

    public List<com.IntegrityTechnologies.business_manager.modules.stock.product.model.ProductAudit> getAuditTrail(UUID productId) {
        return productAuditRepository.findByProductIdOrderByTimestampDesc(productId);
    }

    // Snapshot and historical snapshot handling (kept similar to your previous logic)
    @Transactional
    public void takeSnapshot(LocalDate date) {
        List<InventoryItem> items = inventoryItemRepository.findAll();

        for (InventoryItem i : items) {
            BigDecimal valuationUnit = Optional.ofNullable(i.getAverageCost()).orElse(i.getProductVariant().getProduct().getPrice() != null ? i.getProductVariant().getProduct().getPrice() : BigDecimal.ZERO);
            var snap = InventorySnapshot.builder()
                    .productId(i.getProductVariant().getProduct().getId())
                    .branchId(i.getBranch().getId())
                    .quantityOnHand(i.getQuantityOnHand())
                    .quantityReserved(i.getQuantityReserved())
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

            UUID pid = item.getProductVariant().getProduct().getId();
            UUID bid = item.getBranch().getId();

            var last = inventorySnapshotRepository.findTopByProductIdAndBranchIdAndSnapshotDateLessThanEqualOrderBySnapshotDateDesc(
                    pid, bid, date
            );

            long baseOnHand = last.map(InventorySnapshot::getQuantityOnHand).orElse(0L);
            long baseReserved = last.map(InventorySnapshot::getQuantityReserved).orElse(0L);
            LocalDate baseDate = last.map(InventorySnapshot::getSnapshotDate).orElse(LocalDate.EPOCH);

            LocalDateTime baseDateTime = baseDate.atStartOfDay();

            List<StockTransaction> txns =
                    stockTransactionRepository.findBetween(pid, bid, baseDateTime, toDate);

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

    public Map<String, Object> getInventoryValuation() {
        BigDecimal total = BigDecimal.ZERO;
        for (var item : inventoryItemRepository.findAll()) {
            BigDecimal price = Optional.ofNullable(item.getProductVariant().getProduct().getPrice()).orElse(BigDecimal.ZERO);
            total = total.add(price.multiply(BigDecimal.valueOf(item.getQuantityOnHand())));
        }
        Map<String, Object> out = new HashMap<>();
        out.put("totalValuation", total);
        return out;
    }

    // ------------------------
    // Helpers
    // ------------------------

    private String generateVariantSku(Product product) {
        return product.getSku() + "-" + UUID.randomUUID().toString().substring(0, 6).toUpperCase();
    }

    private InventoryResponse buildResponse(InventoryItem item) {
        return InventoryResponse.builder()
                .productId(item.getProductVariant().getProduct().getId())
                .productName(item.getProductVariant().getProduct().getName())
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
        var createAudit = new com.IntegrityTechnologies.business_manager.modules.stock.product.model.ProductAudit();
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