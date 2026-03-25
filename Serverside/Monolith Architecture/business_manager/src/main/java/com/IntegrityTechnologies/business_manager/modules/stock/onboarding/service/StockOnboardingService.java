package com.IntegrityTechnologies.business_manager.modules.stock.onboarding.service;

import com.IntegrityTechnologies.business_manager.modules.stock.category.repository.CategoryRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.ReceiveStockRequest;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.SupplierUnit;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import com.IntegrityTechnologies.business_manager.modules.stock.onboarding.dto.StockOnboardingRequest;
import com.IntegrityTechnologies.business_manager.modules.stock.onboarding.dto.StockOnboardingResponse;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductCreateDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service.ProductService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.ProductVariantCreateDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.ProductVariantDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.service.ProductVariantService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.model.ProductVariantPackaging;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.service.ProductVariantPackagingService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.ProductPrice;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.repository.ProductPriceRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.service.ProductPriceService;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;

@Service
@RequiredArgsConstructor
public class StockOnboardingService {

    private final ProductService productService;
    private final ProductVariantService variantService;
    private final ProductVariantPackagingService packagingService;
    private final ProductPriceService priceService;
    private final ProductPriceRepository priceRepo;
    private final InventoryService inventoryService;
    private final CategoryRepository categoryRepository;
    private final ProductRepository productRepository;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }
    
    @Transactional
    public StockOnboardingResponse onboard(StockOnboardingRequest req) {

        if (req.getBranchId() == null) {
            throw new IllegalArgumentException("branchId is required");
        }

        if (req.getSuppliers() == null || req.getSuppliers().isEmpty()) {
            throw new IllegalArgumentException("At least one supplier is required");
        }

        // =====================================================
        // 1. PRODUCT
        // =====================================================
        Product product = resolveProduct(req);

        // =====================================================
        // 2. VARIANT
        // =====================================================
        ProductVariantDTO variantDto = resolveVariant(req, product);
        UUID variantId = variantDto.getId();

        // =====================================================
        // 3. PACKAGING (UPSERT)
        // =====================================================
        Map<String, ProductVariantPackaging> packagingMap =
                resolvePackagings(variantId, req);

        ProductVariantPackaging basePackaging =
                packagingService.getBasePackaging(variantId);

        // =====================================================
        // 4. PRICING (UPSERT)
        // =====================================================
        upsertPricing(variantId, packagingMap, req);

        // HARD RULE
        ensureBasePricing(variantId, basePackaging.getId());

        // =====================================================
        // 5. INVENTORY
        // =====================================================
        ReceiveStockRequest receiveReq =
                buildInventoryRequest(req, product.getId(), variantId, packagingMap);

        inventoryService.receiveStock(receiveReq);

        long totalUnits = receiveReq.getSuppliers().stream()
                .mapToLong(SupplierUnit::getUnitsSupplied)
                .sum();

        BigDecimal totalCost = receiveReq.getSuppliers().stream()
                .map(s -> s.getUnitCost().multiply(BigDecimal.valueOf(s.getUnitsSupplied())))
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        return StockOnboardingResponse.builder()
                .productId(product.getId())
                .variantId(variantId)
                .branchId(req.getBranchId())
                .totalUnitsReceived(totalUnits)
                .totalCost(totalCost)
                .message("Stock onboarded successfully")
                .build();
    }

    // =====================================================
    // PRODUCT RESOLUTION
    // =====================================================

    private Product resolveProduct(StockOnboardingRequest req) {

        if (req.getProductId() != null) {
            return productService.getById(req.getBranchId(), req.getProductId());
        }

        if (req.getProductName() == null || req.getProductName().isBlank()) {
            throw new IllegalArgumentException("Product name is required");
        }

        Optional<Product> existing =
                productRepository.findByTenantIdAndBranchIdAndNameIgnoreCase(
                        tenantId(),
                        req.getBranchId(),
                        req.getProductName()
                );

        if (existing.isPresent()) {
            return existing.get();
        }

        if (req.getCategoryId() == null) {
            throw new IllegalArgumentException("CategoryId required for new product");
        }

        ProductCreateDTO dto = ProductCreateDTO.builder()
                .name(req.getProductName())
                .categoryId(req.getCategoryId())
                .supplierIds(req.getSupplierIds())
                .minimumPercentageProfit(req.getMinimumPercentageProfit())
                .build();

        ProductDTO created = productService.createProductCore(req.getBranchId(), dto);
        return productService.getById(req.getBranchId(), created.getId());
    }

    // =====================================================
    // VARIANT
    // =====================================================

    private ProductVariantDTO resolveVariant(
            StockOnboardingRequest req,
            Product product
    ) {
        if (req.getVariantId() != null) {
            return variantService.getVariant(req.getVariantId());
        }

        ProductVariantCreateDTO dto = new ProductVariantCreateDTO();
        dto.setProductId(product.getId());
        dto.setClassification(req.getClassification());

        return variantService.createVariant(dto);
    }

    // =====================================================
    // PACKAGING (UPSERT)
    // =====================================================

    private Map<String, ProductVariantPackaging> resolvePackagings(
            UUID variantId,
            StockOnboardingRequest req
    ) {

        Map<String, ProductVariantPackaging> map = new HashMap<>();

        List<ProductVariantPackaging> existing =
                packagingService.getPackagings(variantId);

        if (req.getPackagings() != null) {
            Set<String> seen = new HashSet<>();

            for (var p : req.getPackagings()) {
                String key = normalize(p.getName());

                if (!seen.add(key)) {
                    throw new IllegalArgumentException("Duplicate packaging name: " + p.getName());
                }
            }
        }

        for (ProductVariantPackaging p : existing) {
            map.put(normalize(p.getName()), p);
        }

        if (req.getPackagings() != null) {
            for (var p : req.getPackagings()) {

                String key = normalize(p.getName());

                if (map.containsKey(key)) continue;

                ProductVariantPackaging created =
                        packagingService.createPackaging(
                                variantId,
                                p.getName(),
                                p.getUnits()
                        );

                map.put(key, created);
            }
        }

        return map;
    }

    // =====================================================
    // PRICING (UPSERT)
    // =====================================================

    private void upsertPricing(
            UUID variantId,
            Map<String, ProductVariantPackaging> packagingMap,
            StockOnboardingRequest req
    ) {

        if (req.getPricing() == null || req.getPricing().isEmpty()) {
            throw new IllegalArgumentException("Pricing required");
        }

        for (var price : req.getPricing()) {

            String key = normalize(price.getPackagingName());

            ProductVariantPackaging pkg = packagingMap.get(key);

            if (pkg == null) {
                throw new IllegalArgumentException(
                        "Unknown packaging: " + price.getPackagingName()
                );
            }

            List<ProductPrice> existing =
                    priceRepo.findByProductVariantIdAndPackagingIdAndDeletedFalse(
                            variantId,
                            pkg.getId()
                    );

            if (!existing.isEmpty()) {
                ProductPrice p = existing.stream()
                        .filter(e -> e.getMinQuantity() == 1L)
                        .findFirst()
                        .orElse(null);

                if (p != null) {
                    p.setPrice(price.getSellingPrice());
                    priceRepo.save(p);
                } else {
                    priceService.createPrice(
                            variantId,
                            pkg.getId(),
                            price.getSellingPrice(),
                            1L
                    );
                }

                p.setPrice(price.getSellingPrice());
                priceRepo.save(p);
            } else {
                priceService.createPrice(
                        variantId,
                        pkg.getId(),
                        price.getSellingPrice(),
                        1L
                );
            }
        }
    }

    private void ensureBasePricing(UUID variantId, UUID basePackagingId) {

        List<ProductPrice> prices =
                priceRepo.findByProductVariantIdAndPackagingIdAndDeletedFalse(
                        variantId,
                        basePackagingId
                );

        if (prices.isEmpty()) {
            throw new IllegalStateException("Base packaging must have pricing");
        }
    }

    // =====================================================
    // INVENTORY
    // =====================================================

    private ReceiveStockRequest buildInventoryRequest(
            StockOnboardingRequest req,
            UUID productId,
            UUID variantId,
            Map<String, ProductVariantPackaging> packagingMap
    ) {

        ReceiveStockRequest r = new ReceiveStockRequest();

        r.setProductId(productId);
        r.setProductVariantId(variantId);
        r.setBranchId(req.getBranchId());
        r.setReference(req.getReference());
        r.setNote(req.getNote());

        List<SupplierUnit> supplierUnits = new ArrayList<>();

        for (var s : req.getSuppliers()) {

            ProductVariantPackaging pkg =
                    packagingMap.get(normalize(s.getPackagingName()));

            if (pkg == null) {
                throw new IllegalArgumentException("Unknown packaging");
            }

            long baseUnits = s.getUnitsSupplied() * pkg.getUnitsPerPackaging();

            BigDecimal unitCost =
                    s.getUnitCost().divide(
                            BigDecimal.valueOf(pkg.getUnitsPerPackaging()),
                            6,
                            RoundingMode.HALF_UP
                    );

            SupplierUnit su = new SupplierUnit();
            su.setSupplierId(s.getSupplierId());
            su.setUnitsSupplied(baseUnits);
            su.setUnitCost(unitCost);

            supplierUnits.add(su);
        }

        r.setSuppliers(supplierUnits);
        return r;
    }

    private String normalize(String s) {
        return s.trim().toLowerCase();
    }
}