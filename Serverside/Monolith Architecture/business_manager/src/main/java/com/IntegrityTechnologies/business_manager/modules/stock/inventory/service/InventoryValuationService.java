package com.IntegrityTechnologies.business_manager.modules.stock.inventory.service;

import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryItem;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventorySnapshot;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.StockTransaction;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryItemRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventorySnapshotRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.StockTransactionRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;

@Service
@RequiredArgsConstructor
public class InventoryValuationService {

    private final InventoryItemRepository inventoryItemRepository;
    private final StockTransactionRepository stockTransactionRepository;
    private final InventorySnapshotRepository snapshotRepository;
    private final Environment env;

    public enum ValuationMethod {
        FIFO, LIFO, WAC, STANDARD
    }

    private ValuationMethod resolveMethod(String override) {
        if (override != null && !override.isBlank()) {
            return ValuationMethod.valueOf(override.toUpperCase());
        }
        String mode = env.getProperty("inventory.valuation.mode", "WAC");
        return ValuationMethod.valueOf(mode.toUpperCase());
    }

    /* ============================================================
       0. TOTAL VALUATION (ALL PRODUCTS, VARIANTS, BRANCHES)
       ============================================================ */
    public Map<String, Object> getTotalValuation() {
        ValuationMethod method = resolveMethod(null);

        BigDecimal total = BigDecimal.ZERO;

        for (InventoryItem item : inventoryItemRepository.findAll()) {
            total = total.add(calculateItemValuation(item, method));
        }

        Map<String, Object> map = new HashMap<>();
        map.put("valuationMethod", method.toString());
        map.put("totalValuation", total);
        map.put("currency", "KES");
        return map;
    }

    /* ============================================================
       1. VALUATION BY PRODUCT (all variants, all branches)
       ============================================================ */
    public Map<String, Object> getProductValuation(UUID productId) {
        ValuationMethod method = resolveMethod(null);
        BigDecimal total = BigDecimal.ZERO;

        List<InventoryItem> items = inventoryItemRepository.findAll()
                .stream()
                .filter(i -> i.getProductVariant() != null &&
                        i.getProductVariant().getProduct().getId().equals(productId))
                .toList();

        for (InventoryItem item : items) {
            total = total.add(calculateItemValuation(item, method));
        }

        Map<String, Object> map = new HashMap<>();
        map.put("productId", productId);
        map.put("valuationMethod", method.toString());
        map.put("totalValuation", total);
        return map;
    }

    /* ============================================================
       2. VALUATION BY BRANCH
       ============================================================ */
    public Map<String, Object> getBranchValuation(UUID branchId) {
        ValuationMethod method = resolveMethod(null);
        BigDecimal total = BigDecimal.ZERO;

        List<InventoryItem> items = inventoryItemRepository.findByBranchId(branchId);

        for (InventoryItem item : items) {
            total = total.add(calculateItemValuation(item, method));
        }

        Map<String, Object> map = new HashMap<>();
        map.put("branchId", branchId);
        map.put("valuationMethod", method.toString());
        map.put("totalValuation", total);
        return map;
    }

    /* ============================================================
       3. CATEGORY-LEVEL VALUATION
       ============================================================ */
    public Map<String, Object> getCategoryValuation() {
        ValuationMethod method = resolveMethod(null);

        Map<String, BigDecimal> totals = new HashMap<>();

        for (InventoryItem item : inventoryItemRepository.findAll()) {

            Category cat = item.getProductVariant()
                    .getProduct()
                    .getCategory();

            if (cat == null || item.getQuantityOnHand() <= 0) continue;

            BigDecimal val = calculateItemValuation(item, method);

            totals.merge(cat.getName(), val, BigDecimal::add);
        }

        Map<String, Object> out = new HashMap<>();
        out.put("valuationMethod", method.toString());
        out.put("categories", totals);
        return out;
    }

    /* ============================================================
       4. HISTORICAL VALUATION ("as of date")
       ============================================================ */
    public Map<String, Object> getHistoricalValuation(LocalDate date, String methodOverride) {

        ValuationMethod method = resolveMethod(methodOverride);

        BigDecimal total = BigDecimal.ZERO;

        List<InventoryItem> items = inventoryItemRepository.findAll();

        for (InventoryItem item : items) {

            UUID variantId = item.getProductVariant().getId();
            UUID branchId = item.getBranch().getId();

            long qty = reconstructHistoricalQty(variantId, branchId, date);

            BigDecimal val = switch (method) {
                case FIFO -> fifoValuation(variantId, branchId, qty);
                case LIFO -> lifoValuation(variantId, branchId, qty);
                case WAC -> wacValuation(variantId, branchId, qty);
                case STANDARD -> {
                    BigDecimal std = new BigDecimal(env.getProperty("inventory.valuation.standard-price", "0"));
                    yield std.multiply(BigDecimal.valueOf(qty));
                }
            };

            total = total.add(val);
        }

        Map<String, Object> out = new HashMap<>();
        out.put("date", date.toString());
        out.put("valuationMethod", method.toString());
        out.put("totalValuation", total);
        return out;
    }

    /* ---------------------------------------------------------
       Reconstruct inventory qty as-of a historical date
       --------------------------------------------------------- */
    private long reconstructHistoricalQty(UUID variantId, UUID branchId, LocalDate date) {

        Optional<InventorySnapshot> snapOpt =
                snapshotRepository.findTopByProductVariantIdAndBranchIdAndSnapshotDateLessThanEqualOrderBySnapshotDateDesc(
                        variantId, branchId, date
                );

        long baseOnHand = snapOpt.map(InventorySnapshot::getQuantityOnHand).orElse(0L);
        LocalDate snapDate = snapOpt.map(InventorySnapshot::getSnapshotDate).orElse(LocalDate.EPOCH);

        LocalDateTime from = snapDate.atStartOfDay();
        LocalDateTime to = date.atTime(23, 59, 59);

        List<StockTransaction> txns =
                stockTransactionRepository.findBetweenVariant(variantId, branchId, from, to);

        for (StockTransaction t : txns) {
            switch (t.getType()) {
                case RECEIPT, ADJUSTMENT -> baseOnHand += t.getQuantityDelta();
                case SALE -> baseOnHand -= t.getQuantityDelta();
                default -> {}
            }
        }

        return baseOnHand;
    }

    /* ============================================================
       5. Core valuation method
       ============================================================ */
    public BigDecimal calculateItemValuation(InventoryItem item, ValuationMethod method) {

        long qty = item.getQuantityOnHand();
        if (qty <= 0) return BigDecimal.ZERO;

        UUID variantId = item.getProductVariant().getId();
        UUID branchId = item.getBranch().getId();

        BigDecimal unitCost = switch (method) {
            case FIFO, LIFO, WAC -> resolveUnitCost(variantId, branchId);
            case STANDARD -> new BigDecimal(
                    env.getProperty("inventory.valuation.standard-price", "0")
            );
        };

        return unitCost.multiply(BigDecimal.valueOf(qty));
    }

    /* ============================================================
       FIFO
       ============================================================ */
    private BigDecimal fifoValuation(UUID variantId, UUID branchId, long qtyOnHand) {

        List<StockTransaction> receipts =
                stockTransactionRepository.findReceipts(variantId, branchId);

        receipts.sort(Comparator.comparing(StockTransaction::getTimestamp));

        BigDecimal total = BigDecimal.ZERO;
        long remaining = qtyOnHand;

        for (StockTransaction r : receipts) {
            if (remaining <= 0) break;
            long used = Math.min(remaining, r.getQuantityDelta());
            total = total.add(r.getUnitCost().multiply(BigDecimal.valueOf(used)));
            remaining -= used;
        }
        return total;
    }

    /* ============================================================
       LIFO
       ============================================================ */
    private BigDecimal lifoValuation(UUID variantId, UUID branchId, long qtyOnHand) {

        List<StockTransaction> receipts =
                stockTransactionRepository.findReceipts(variantId, branchId);

        receipts.sort(Comparator.comparing(StockTransaction::getTimestamp).reversed());

        BigDecimal total = BigDecimal.ZERO;
        long remaining = qtyOnHand;

        for (StockTransaction r : receipts) {
            if (remaining <= 0) break;
            long used = Math.min(remaining, r.getQuantityDelta());
            total = total.add(r.getUnitCost().multiply(BigDecimal.valueOf(used)));
            remaining -= used;
        }
        return total;
    }

    /* ============================================================
       WAC
       ============================================================ */
    private BigDecimal wacValuation(UUID variantId, UUID branchId, long qtyOnHand) {

        List<StockTransaction> receipts =
                stockTransactionRepository.findReceipts(variantId, branchId);

        BigDecimal totalValue = BigDecimal.ZERO;
        long totalUnits = 0;

        for (StockTransaction r : receipts) {
            totalValue = totalValue.add(r.getUnitCost().multiply(BigDecimal.valueOf(r.getQuantityDelta())));
            totalUnits += r.getQuantityDelta();
        }

        if (totalUnits == 0) return BigDecimal.ZERO;

        BigDecimal avgCost = totalValue.divide(BigDecimal.valueOf(totalUnits), 6, RoundingMode.HALF_UP);

        return avgCost.multiply(BigDecimal.valueOf(qtyOnHand));
    }

    public Map<String, Object> getInventoryValuation() {

        ValuationMethod method = resolveMethod(null);

        BigDecimal total = BigDecimal.ZERO;

        for (InventoryItem item : inventoryItemRepository.findAll()) {
            total = total.add(calculateItemValuation(item, method));
        }

        Map<String, Object> out = new HashMap<>();
        out.put("valuationMethod", method.toString());
        out.put("totalValuation", total);
        out.put("currency", "KES");

        return out;
    }

    public Map<UUID, BigDecimal> getAllBranchesValuation() {
        ValuationMethod method = resolveMethod(null);

        Map<UUID, BigDecimal> map = new HashMap<>();

        for (InventoryItem item : inventoryItemRepository.findAll()) {
            UUID branchId = item.getBranch().getId();
            BigDecimal val = calculateItemValuation(item, method);
            map.merge(branchId, val, BigDecimal::add);
        }

        return map;
    }

    public List<Map<String, Object>> getTopValuedProducts(int limit) {

        ValuationMethod method = resolveMethod(null);

        Map<UUID, BigDecimal> totals = new HashMap<>();

        for (InventoryItem item : inventoryItemRepository.findAll()) {
            UUID productId = item.getProductVariant().getProduct().getId();
            BigDecimal val = calculateItemValuation(item, method);
            totals.merge(productId, val, BigDecimal::add);
        }

        return totals.entrySet().stream()
                .sorted(Map.Entry.<UUID, BigDecimal>comparingByValue().reversed())
                .limit(limit)
                .map(e -> {
                    Map<String, Object> m = new HashMap<>();
                    m.put("productId", e.getKey());
                    m.put("valuation", e.getValue());
                    return m;
                })
                .toList();
    }

    public String resolveCurrentMethod() {
        return resolveMethod(null).toString();
    }

    private BigDecimal resolveUnitCost(UUID variantId, UUID branchId) {

        // 1️⃣ Try WAC from receipts
        List<StockTransaction> receipts =
                stockTransactionRepository.findReceipts(variantId, branchId);

        if (!receipts.isEmpty()) {
            BigDecimal totalValue = BigDecimal.ZERO;
            long totalQty = 0;

            for (StockTransaction r : receipts) {
                if (r.getUnitCost() == null) continue;
                totalValue = totalValue.add(
                        r.getUnitCost().multiply(BigDecimal.valueOf(r.getQuantityDelta()))
                );
                totalQty += r.getQuantityDelta();
            }

            if (totalQty > 0) {
                return totalValue.divide(
                        BigDecimal.valueOf(totalQty),
                        6,
                        RoundingMode.HALF_UP
                );
            }
        }

        // 2️⃣ Fallback → variant averageBuyingPrice
        return inventoryItemRepository.findFirstByProductVariantId(variantId)
                .map(i -> i.getProductVariant().getAverageBuyingPrice())
                .orElse(BigDecimal.ZERO);
    }

}