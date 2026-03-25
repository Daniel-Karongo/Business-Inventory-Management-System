package com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.valuation;

import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryItem;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventorySnapshot;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryItemRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventorySnapshotRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.StockTransactionRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.TenantInventorySettingsService;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class InventoryValuationService {

    private final InventoryItemRepository inventoryItemRepository;
    private final InventorySnapshotRepository snapshotRepository;
    private final Environment env;
    private final InventoryValuationEngine valuationEngine;
    private final TenantInventorySettingsService settingsService;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    public enum ValuationMethod {
        FIFO, LIFO, WAC, STANDARD
    }

    private ValuationMethod resolveMethod(String override) {

        if (override != null && !override.isBlank()) {
            return ValuationMethod.valueOf(override.toUpperCase());
        }

        return settingsService.get(tenantId());
    }

    /* ============================================================ */
    public Map<String, Object> getTotalValuation() {
        ValuationMethod method = resolveMethod(null);

        BigDecimal total = BigDecimal.ZERO;

        for (InventoryItem item : inventoryItemRepository.findByTenantIdAndDeletedFalse(tenantId())) {
            total = total.add(calculateItemValuation(item, method));
        }

        return Map.of(
                "valuationMethod", method.toString(),
                "totalValuation", total,
                "currency", "KES"
        );
    }

    /* ============================================================ */
    public Map<String, Object> getProductValuation(UUID productId) {

        ValuationMethod method = resolveMethod(null);
        BigDecimal total = BigDecimal.ZERO;

        List<InventoryItem> items = inventoryItemRepository
                .findByTenantIdAndDeletedFalse(tenantId());

        for (InventoryItem item : items) {
            if (item.getProductVariant() != null &&
                    item.getProductVariant().getProduct().getId().equals(productId)) {

                total = total.add(calculateItemValuation(item, method));
            }
        }

        return Map.of(
                "productId", productId,
                "valuationMethod", method.toString(),
                "totalValuation", total
        );
    }

    /* ============================================================ */
    public Map<String, Object> getBranchValuation(UUID branchId) {

        ValuationMethod method = resolveMethod(null);
        BigDecimal total = BigDecimal.ZERO;

        List<InventoryItem> items =
                inventoryItemRepository.findByTenantIdAndBranchIdAndDeletedFalse(
                        tenantId(),
                        branchId
                );

        for (InventoryItem item : items) {
            total = total.add(calculateItemValuation(item, method));
        }

        return Map.of(
                "branchId", branchId,
                "valuationMethod", method.toString(),
                "totalValuation", total
        );
    }

    /* ============================================================ */
    public Map<String, Object> getCategoryValuation() {

        ValuationMethod method = resolveMethod(null);
        Map<String, BigDecimal> totals = new HashMap<>();

        for (InventoryItem item : inventoryItemRepository.findByTenantIdAndDeletedFalse(tenantId())) {

            Category cat = item.getProductVariant()
                    .getProduct()
                    .getCategory();

            if (cat == null || item.getQuantityOnHand() <= 0) continue;

            BigDecimal val = calculateItemValuation(item, method);

            totals.merge(cat.getName(), val, BigDecimal::add);
        }

        return Map.of(
                "valuationMethod", method.toString(),
                "categories", totals
        );
    }

    /* ============================================================ */
    public BigDecimal getHistoricalValuation(UUID variantId, UUID branchId, LocalDate date) {

        return snapshotRepository.findTopByProductVariantIdAndBranchIdAndTenantIdAndSnapshotDateLessThanEqualOrderBySnapshotDateDesc(
                        variantId,
                        branchId,
                        tenantId(),
                        date
                )
                .map(InventorySnapshot::getValuation)
                .orElse(BigDecimal.ZERO);
    }

    public Map<String, Object> getHistoricalValuation(LocalDate date, String methodOverride) {
        ValuationMethod method = resolveMethod(methodOverride);

        BigDecimal total = BigDecimal.ZERO;

        for (InventoryItem item : inventoryItemRepository.findByTenantIdAndDeletedFalse(tenantId())) {

            BigDecimal val = snapshotRepository.findTopByProductVariantIdAndBranchIdAndTenantIdAndSnapshotDateLessThanEqualOrderBySnapshotDateDesc(
                            item.getProductVariant().getId(),
                            item.getBranchId(),
                            tenantId(),
                            date
                    )
                    .map(InventorySnapshot::getValuation)
                    .orElse(BigDecimal.ZERO);

            total = total.add(val);
        }

        return Map.of(
                "date", date,
                "valuationMethod", method.toString(),
                "totalValuation", total
        );
    }

    /* ============================================================ */
    public BigDecimal calculateItemValuation(InventoryItem item, ValuationMethod method) {

        long qty = item.getQuantityOnHand();
        if (qty <= 0) return BigDecimal.ZERO;

        return valuationEngine.valuate(
                method.name(),
                item.getProductVariant().getId(),
                item.getBranchId(),
                qty
        );
    }

    /* ============================================================ */

    public Map<UUID, BigDecimal> getAllBranchesValuation() {

        ValuationMethod method = resolveMethod(null);
        Map<UUID, BigDecimal> map = new HashMap<>();

        for (InventoryItem item : inventoryItemRepository.findByTenantIdAndDeletedFalse(tenantId())) {
            UUID branchId = item.getBranchId();
            BigDecimal val = calculateItemValuation(item, method);
            map.merge(branchId, val, BigDecimal::add);
        }

        return map;
    }

    public List<Map<String, Object>> getTopValuedProducts(int limit) {

        ValuationMethod method = resolveMethod(null);
        Map<UUID, BigDecimal> totals = new HashMap<>();

        for (InventoryItem item : inventoryItemRepository.findByTenantIdAndDeletedFalse(tenantId())) {
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
}