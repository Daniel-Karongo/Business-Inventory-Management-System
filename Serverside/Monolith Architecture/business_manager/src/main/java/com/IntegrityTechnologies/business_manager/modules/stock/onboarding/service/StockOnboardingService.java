package com.IntegrityTechnologies.business_manager.modules.stock.onboarding.service;

import com.IntegrityTechnologies.business_manager.config.caffeine.CacheInvalidationService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.Account;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.dto.CreateOperationalExpenseRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.service.OperationalExpenseService;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.dto.ProcessSupplierPaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.enums.SupplierPaymentMethod;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.service.SupplierPaymentProcessingService;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.base.config.TaxProperties;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto.VatBreakdown;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.service.VatCalculationService;
import com.IntegrityTechnologies.business_manager.modules.person.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.person.supplier.repository.SupplierRepository;
import com.IntegrityTechnologies.business_manager.modules.person.supplier.service.SupplierService;
import com.IntegrityTechnologies.business_manager.modules.procurement.matching.dto.ReceiveAndInvoiceRequest;
import com.IntegrityTechnologies.business_manager.modules.procurement.matching.dto.ReceiveAndInvoiceResult;
import com.IntegrityTechnologies.business_manager.modules.procurement.matching.service.ReceiveAndInvoiceService;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.stock.category.repository.CategoryRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.category.service.CategoryService;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.ReceiveStockRequest;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.SupplierUnit;
import com.IntegrityTechnologies.business_manager.modules.stock.onboarding.dto.StockOnboardingBulkPreviewRow;
import com.IntegrityTechnologies.business_manager.modules.stock.onboarding.dto.StockOnboardingRequest;
import com.IntegrityTechnologies.business_manager.modules.stock.onboarding.dto.StockOnboardingResponse;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductCreateDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service.ProductService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.ProductVariantCreateDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.ProductVariantDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.repository.ProductVariantRepository;
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
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class StockOnboardingService {

    private final ProductService productService;
    private final ProductVariantService variantService;
    private final ProductVariantPackagingService packagingService;
    private final ProductPriceService priceService;
    private final ProductPriceRepository priceRepo;
    private final ProductRepository productRepository;
    private final CacheInvalidationService cacheInvalidationService;
    private final CategoryRepository categoryRepository;
    private final CategoryService categoryService;
    private final SupplierRepository supplierRepository;
    private final SupplierService supplierService;
    private final ProductVariantRepository variantRepo;
    private final ReceiveAndInvoiceService receiveAndInvoiceService;
    private final TaxProperties taxProperties;
    private final VatCalculationService vatCalculationService;
    private final OperationalExpenseService operationalExpenseService;
    private final SupplierPaymentProcessingService supplierPaymentProcessingService;
    private final AccountRepository accountRepository;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    @Transactional
    public StockOnboardingResponse onboard(StockOnboardingRequest req) {

        validateRequest(req);
        validateFundingAccountPaymentMethod(req);
        ensureExplicitBasePackaging(req);

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

        // =====================================================
        // 4. PRICING (UPSERT)
        // =====================================================
        upsertPricing(variantId, packagingMap, req);

//        // HARD RULE
//        ensureBasePricing(
//                variantId,
//                basePackaging.getId()
//        );

        // =====================================================
        // 5. INVENTORY
        // =====================================================
        ReceiveStockRequest receiveReq =
                buildInventoryRequest(req, product.getId(), variantId, packagingMap);

        ReceiveAndInvoiceRequest orchestration =
                new ReceiveAndInvoiceRequest();

        orchestration.setStockReceipt(
                receiveReq
        );

        orchestration.setAccountingDate(
                req.getAccountingDate()
        );

//        BigDecimal supplierTotal =
//                Boolean.TRUE.equals(
//                        req.getAutoPaySuppliers()
//                )
//                        ? totalSupplierCost(req)
//                        : BigDecimal.ZERO;
//
//        BigDecimal expenseTotal =
//                Boolean.TRUE.equals(
//                        req.getAutoPayOperationalExpenses()
//                )
//                        ? totalOperationalExpenses(req)
//                        : BigDecimal.ZERO;
//
//        BigDecimal requiredFunding =
//                supplierTotal.add(
//                        expenseTotal
//                );
//
//        if (
//                requiredFunding.compareTo(BigDecimal.ZERO) > 0
//        ) {
//            autoFundingService.ensureFundingAvailable(
//                    req.getBranchId(),
//                    req.getFundingAccountId(),
//                    requiredFunding,
//                    req.getAccountingDate(),
//                    req.getReference()
//            );
//        }

        ReceiveAndInvoiceResult result =
                receiveAndInvoiceService.execute(
                        orchestration
                );

        autoPaySuppliers(
                req,
                product,
                result.getSupplierInvoiceIds()
        );

        processOperationalExpenses(
                req
        );

        long totalUnits = receiveReq.getSuppliers().stream()
                .mapToLong(SupplierUnit::getUnitsSupplied)
                .sum();

        BigDecimal totalCost = receiveReq.getSuppliers().stream()
                .map(s -> s.getUnitCost().multiply(BigDecimal.valueOf(s.getUnitsSupplied())))
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        evictAll(variantId, req.getBranchId());

        return StockOnboardingResponse.builder()
                .productId(product.getId())
                .variantId(variantId)
                .branchId(req.getBranchId())
                .totalUnitsReceived(totalUnits)
                .totalCost(totalCost)
                .message("Stock onboarded successfully")
                .build();
    }

    @Transactional(readOnly = true)
    public StockOnboardingBulkPreviewRow preview(
            StockOnboardingRequest req
    ) {

        validateRequest(req);
        validateFundingAccountPaymentMethod(req);

        Product existingProduct = null;

        boolean existingProductFlag = false;

        if (req.getProductId() != null) {

            existingProduct =
                    productService.getById(
                            req.getBranchId(),
                            req.getProductId()
                    );

            existingProductFlag = true;

        } else if (req.getProductName() != null) {

            existingProduct =
                    productRepository
                            .findByTenantIdAndBranchIdAndNameIgnoreCase(
                                    tenantId(),
                                    req.getBranchId(),
                                    req.getProductName().trim()
                            )
                            .orElse(null);

            existingProductFlag =
                    existingProduct != null;
        }

        boolean existingVariantFlag =
                req.getVariantId() != null;

        long totalUnits = 0;

        BigDecimal grossCost = BigDecimal.ZERO;
        BigDecimal netCost = BigDecimal.ZERO;
        BigDecimal vatAmount = BigDecimal.ZERO;

        int suppliersCreated = 0;

        for (var s : req.getSuppliers()) {

            long units =
                    s.getUnitsSupplied() != null
                            ? s.getUnitsSupplied()
                            : 0;

            BigDecimal cost =
                    s.getUnitCost() != null
                            ? s.getUnitCost()
                            : BigDecimal.ZERO;

            totalUnits += units;

            BigDecimal gross =
                    cost.multiply(
                            BigDecimal.valueOf(units)
                    );

            boolean vatInclusive =
                    s.getVatInclusive() != null
                            ? s.getVatInclusive()
                            : taxProperties.isPricesVatInclusive();

            BigDecimal vatRate =
                    s.getVatRate() != null
                            ? s.getVatRate()
                            : taxProperties.getVatRate();

            VatBreakdown breakdown =
                    taxProperties.isVatEnabled()
                            ? vatCalculationService.calculate(
                            gross,
                            vatInclusive,
                            vatRate
                    )
                            : new VatBreakdown(
                            gross,
                            BigDecimal.ZERO,
                            gross
                    );

            grossCost =
                    grossCost.add(
                            breakdown.gross()
                    );

            netCost =
                    netCost.add(
                            breakdown.net()
                    );

            vatAmount =
                    vatAmount.add(
                            breakdown.vat()
                    );

            if (s.getSupplierId() == null
                    && s.getSupplierName() != null) {

                boolean exists =
                        supplierRepository.findByNameSafe(
                                s.getSupplierName().trim(),
                                false,
                                tenantId(),
                                req.getBranchId()
                        ).isPresent();

                if (!exists) {
                    suppliersCreated++;
                }
            }
        }

        boolean categoryCreated = false;

        if (req.getCategoryId() == null
                && req.getNewCategoryName() != null) {

            categoryCreated =
                    categoryRepository.findByNameSafe(
                            req.getNewCategoryName().trim(),
                            false,
                            tenantId(),
                            req.getBranchId()
                    ).isEmpty();
        }

        return StockOnboardingBulkPreviewRow.builder()
                .productId(
                        existingProduct != null
                                ? existingProduct.getId()
                                : null
                )
                .productName(req.getProductName())
                .variantId(req.getVariantId())
                .classification(req.getClassification())
                .branchId(req.getBranchId())
                .totalUnits(totalUnits)
                .grossCost(grossCost)
                .netCost(netCost)
                .vatAmount(vatAmount)
                .existingProduct(existingProductFlag)
                .existingVariant(existingVariantFlag)
                .categoryCreated(categoryCreated)
                .suppliersCreated(suppliersCreated)
                .packagingCount(
                        req.getPackagings() != null
                                ? req.getPackagings().size()
                                : 0
                )
                .pricingCount(
                        req.getPricing() != null
                                ? req.getPricing().size()
                                : 0
                )
                .build();
    }

    private void evictAll(UUID variantId, UUID branchId) {
        cacheInvalidationService.evictPricingByVariant(tenantId(), variantId);
        cacheInvalidationService.evictPackaging(tenantId(), variantId);
        cacheInvalidationService.evictVariantSearch(tenantId(), branchId);
        cacheInvalidationService.evictBarcode(tenantId(), branchId);
    }

    // =====================================================
    // PRODUCT RESOLUTION
    // =====================================================

    private Product resolveProduct(StockOnboardingRequest req) {

        if (req.getBranchId() == null) {
            throw new IllegalArgumentException("branchId is required");
        }

    /* =====================================================
       EXISTING PRODUCT
    ===================================================== */

        if (req.getProductId() != null) {
            return productService.getById(
                    req.getBranchId(),
                    req.getProductId()
            );
        }

    /* =====================================================
       VALIDATE NAME
    ===================================================== */

        if (req.getProductName() == null
                || req.getProductName().isBlank()) {
            throw new IllegalArgumentException(
                    "productName is required"
            );
        }

        String normalizedProductName =
                req.getProductName().trim();

    /* =====================================================
       EXISTING PRODUCT BY NAME
    ===================================================== */

        Optional<Product> existing =
                productRepository.findByNameForUpdate(
                        tenantId(),
                        req.getBranchId(),
                        normalizedProductName
                );

        if (existing.isPresent()) {
            return existing.get();
        }

    /* =====================================================
       CREATE NEW PRODUCT
    ===================================================== */

        boolean createProduct =
                req.getCreateProductIfMissing() == null
                        || req.getCreateProductIfMissing();

        if (!createProduct) {
            throw new IllegalArgumentException(
                    "Product does not exist: " + normalizedProductName
            );
        }

        Category category = resolveCategory(req);

        List<UUID> resolvedSupplierIds =
                resolveProductSupplierIds(
                        req,
                        category
                );

        ProductCreateDTO dto = ProductCreateDTO.builder()
                .name(normalizedProductName)
                .categoryId(category.getId())
                .supplierIds(
                        resolvedSupplierIds.isEmpty()
                                ? null
                                : resolvedSupplierIds
                )
                .minimumPercentageProfit(
                        req.getMinimumPercentageProfit()
                )
                .build();

        ProductDTO created =
                productService.createProductCore(
                        req.getBranchId(),
                        dto
                );

        return productService.getById(
                req.getBranchId(),
                created.getId()
        );
    }

    private Category resolveCategory(
            StockOnboardingRequest req
    ) {

    /* =====================================================
       CATEGORY ID
    ===================================================== */

        if (req.getCategoryId() != null) {

            return categoryRepository
                    .findByIdSafe(
                            req.getCategoryId(),
                            false,
                            tenantId(),
                            req.getBranchId()
                    )
                    .orElseThrow(() ->
                            new IllegalArgumentException(
                                    "Invalid categoryId"
                            )
                    );
        }

    /* =====================================================
       CATEGORY NAME
    ===================================================== */

        if (req.getNewCategoryName() == null
                || req.getNewCategoryName().isBlank()) {

            throw new IllegalArgumentException(
                    "categoryId or newCategoryName is required"
            );
        }

        String normalized =
                req.getNewCategoryName().trim();

        Optional<Category> existing =
                categoryRepository.findByNameSafe(
                        normalized,
                        false,
                        tenantId(),
                        req.getBranchId()
                );

        if (existing.isPresent()) {
            return existing.get();
        }

        boolean createMissing =
                req.getCreateCategoryIfMissing() == null
                        || req.getCreateCategoryIfMissing();

        if (!createMissing) {
            throw new IllegalArgumentException(
                    "Category does not exist: " + normalized
            );
        }

        return categoryService.createMinimal(
                normalized,
                req.getBranchId()
        );
    }

    private List<UUID> resolveProductSupplierIds(
            StockOnboardingRequest req,
            Category category
    ) {

        if (req.getSuppliers() == null
                || req.getSuppliers().isEmpty()) {
            return List.of();
        }

        Set<UUID> supplierIds = new LinkedHashSet<>();

        for (var supplierInput : req.getSuppliers()) {

            UUID supplierId =
                    resolveSupplier(
                            supplierInput,
                            category,
                            req.getBranchId()
                    );

            supplierIds.add(supplierId);
        }

        return new ArrayList<>(supplierIds);
    }

    private UUID resolveSupplier(
            StockOnboardingRequest.SupplierInput input,
            Category category,
            UUID branchId
    ) {

    /* =====================================================
       EXISTING SUPPLIER ID
    ===================================================== */

        if (input.getSupplierId() != null) {

            Supplier supplier =
                    supplierRepository
                            .findByIdSafe(
                                    input.getSupplierId(),
                                    false,
                                    tenantId(),
                                    branchId
                            )
                            .orElseThrow(() ->
                                    new IllegalArgumentException(
                                            "Invalid supplierId"
                                    )
                            );

            return supplier.getId();
        }

    /* =====================================================
       SUPPLIER NAME
    ===================================================== */

        if (input.getSupplierName() == null
                || input.getSupplierName().isBlank()) {

            throw new IllegalArgumentException(
                    "supplierId or supplierName is required"
            );
        }

        String normalized =
                input.getSupplierName().trim();

        Optional<Supplier> existing =
                supplierRepository.findByNameSafe(
                        normalized,
                        false,
                        tenantId(),
                        branchId
                );

        if (existing.isPresent()) {
            return existing.get().getId();
        }

        boolean createMissing =
                input.getCreateSupplierIfMissing() == null
                        || input.getCreateSupplierIfMissing();

        if (!createMissing) {
            throw new IllegalArgumentException(
                    "Supplier does not exist: " + normalized
            );
        }

        Supplier created =
                supplierService.createMinimalSupplier(
                        normalized,
                        category,
                        branchId
                );

        return created.getId();
    }

    // =====================================================
    // VARIANT
    // =====================================================

    private ProductVariantDTO resolveVariant(
            StockOnboardingRequest req,
            Product product
    ) {

        if (req.getVariantId() != null) {
            return variantService.getVariant(
                    req.getBranchId(),
                    req.getVariantId()
            );
        }

        Optional<ProductVariant> existing =
                variantRepo.findByTenantIdAndBranchIdAndProduct_IdAndClassification(
                        tenantId(),
                        req.getBranchId(),
                        product.getId(),
                        req.getClassification()
                );

        if (existing.isPresent()) {
            return variantService.getVariant(
                    req.getBranchId(),
                    existing.get().getId()
            );
        }

        ProductVariantCreateDTO dto =
                new ProductVariantCreateDTO();

        dto.setProductId(product.getId());

        dto.setClassification(
                req.getClassification()
        );
        dto.setAutoCreateBasePackaging(false);

        return variantService.createVariant(
                req.getBranchId(),
                dto
        );
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

        Set<Long> existingUnits = existing.stream()
                .map(ProductVariantPackaging::getUnitsPerPackaging)
                .collect(Collectors.toSet());

        if (req.getPackagings() != null) {

            for (var p : req.getPackagings()) {

                String key = normalize(p.getName());

                if (existingUnits.contains(p.getUnits())) {

                    boolean samePackaging =
                            existing.stream()
                                    .anyMatch(ep ->
                                            ep.getUnitsPerPackaging().equals(p.getUnits())
                                                    && normalize(ep.getName()).equals(key)
                                    );

                    if (!samePackaging) {
                        throw new IllegalArgumentException(
                                "Another packaging already exists with units="
                                        + p.getUnits()
                        );
                    }
                }

                ProductVariantPackaging existingPackaging =
                        map.get(key);

                /*
                 * Packaging already exists.
                 *
                 * Enforce structural equality.
                 */
                if (existingPackaging != null) {

                    Long existingPackagingUnits =
                            existingPackaging.getUnitsPerPackaging();

                    Long requestedUnits =
                            p.getUnits();

                    if (!Objects.equals(
                            existingPackagingUnits,
                            requestedUnits
                    )) {

                        throw new IllegalArgumentException(
                                "Packaging already exists with different units: "
                                        + p.getName()
                                        + " (existing="
                                        + existingPackagingUnits
                                        + ", requested="
                                        + requestedUnits
                                        + ")"
                        );
                    }

                    /*
                     * Same structure.
                     * Reuse existing packaging.
                     */
                    continue;
                }

                /*
                 * Packaging does not exist.
                 * Create it.
                 */
                ProductVariantPackaging created =
                        packagingService.createPackaging(
                                req.getBranchId(),
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

            ProductPrice base =
                    existing.stream()
                            .filter(e -> e.getMinQuantity() == 1L)
                            .findFirst()
                            .orElse(null);

            if (base != null) {
                base.setPrice(price.getSellingPrice());
                priceRepo.save(base);
            } else {
                priceService.createPrice(
                        req.getBranchId(),
                        variantId,
                        pkg.getId(),
                        price.getSellingPrice(),
                        1L
                );
            }
        }
    }

    private void ensureExplicitBasePackaging(
            StockOnboardingRequest req
    ) {

        if (req.getPackagings() == null
                || req.getPackagings().isEmpty()) {

            throw new IllegalArgumentException(
                    "At least one packaging is required"
            );
        }

        boolean hasBasePackaging =
                req.getPackagings()
                        .stream()
                        .anyMatch(p ->
                                p.getUnits() != null
                                        && p.getUnits() == 1L
                        );

        if (!hasBasePackaging) {
            throw new IllegalArgumentException(
                    "A base packaging with units=1 is required"
            );
        }
    }

//    private void ensureBasePricing(
//            UUID variantId,
//            UUID basePackagingId
//    ) {
//
//        List<ProductPrice> prices =
//                priceRepo.findByProductVariantIdAndPackagingIdAndDeletedFalse(
//                        variantId,
//                        basePackagingId
//                );
//
//        boolean hasBasePrice =
//                prices.stream()
//                        .anyMatch(p -> p.getMinQuantity() == 1L);
//
//        if (!hasBasePrice) {
//            throw new IllegalStateException(
//                    "Base packaging must have pricing"
//            );
//        }
//    }

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

        r.setAccountingDate(
                req.getAccountingDate()
        );

        List<SupplierUnit> supplierUnits =
                new ArrayList<>();

        Product product =
                productService.getById(
                        req.getBranchId(),
                        productId
                );

        Category category = product.getCategory();

        for (var s : req.getSuppliers()) {

            ProductVariantPackaging pkg =
                    packagingMap.get(
                            normalize(s.getPackagingName())
                    );

            if (pkg == null) {
                throw new IllegalArgumentException(
                        "Unknown packaging: "
                                + s.getPackagingName()
                );
            }

            UUID supplierId =
                    resolveSupplier(
                            s,
                            category,
                            req.getBranchId()
                    );

            long baseUnits =
                    s.getUnitsSupplied()
                            * pkg.getUnitsPerPackaging();

            BigDecimal unitCost =
                    s.getUnitCost().divide(
                            BigDecimal.valueOf(
                                    pkg.getUnitsPerPackaging()
                            ),
                            6,
                            RoundingMode.HALF_UP
                    );

            SupplierUnit su = new SupplierUnit();

            su.setSupplierId(supplierId);

            su.setUnitsSupplied(baseUnits);

            su.setUnitCost(unitCost);

            su.setVatInclusive(s.getVatInclusive());
            su.setVatRate(s.getVatRate());

            supplierUnits.add(su);
        }

        r.setSuppliers(supplierUnits);

        return r;
    }

    private void autoPaySuppliers(
            StockOnboardingRequest req,
            Product product,
            Map<UUID, List<UUID>> supplierInvoiceIds
    ) {

        if (!Boolean.TRUE.equals(
                req.getAutoPaySuppliers()
        )) {
            return;
        }

        Map<UUID, BigDecimal> supplierTotals =
                new LinkedHashMap<>();

        Category category =
                product.getCategory();

        for (var supplierLine : req.getSuppliers()) {

            UUID supplierId =
                    resolveSupplier(
                            supplierLine,
                            category,
                            req.getBranchId()
                    );

            BigDecimal lineTotal =
                    supplierLine.getUnitCost()
                            .multiply(
                                    BigDecimal.valueOf(
                                            supplierLine.getUnitsSupplied()
                                    )
                            );

            supplierTotals.merge(
                    supplierId,
                    lineTotal,
                    BigDecimal::add
            );
        }

        for (var entry : supplierTotals.entrySet()) {

            ProcessSupplierPaymentRequest payment =
                    new ProcessSupplierPaymentRequest();

            payment.setBranchId(
                    req.getBranchId()
            );

            payment.setSupplierId(
                    entry.getKey()
            );

            payment.setFundingAccountId(
                    req.getFundingAccountId()
            );

            payment.setAmount(
                    entry.getValue()
            );

            payment.setMethod(
                    req.getSupplierPaymentMethod()
            );

            payment.setReference(
                    req.getReference()
                            + "::SUPPLIER::"
                            + entry.getKey()
            );

            payment.setPaymentDate(
                    req.getAccountingDate()
            );

            payment.setTargetInvoiceIds(
                    supplierInvoiceIds.get(
                            entry.getKey()
                    )
            );

            payment.setAutoPost(true);

            payment.setAutoAllocate(true);

            supplierPaymentProcessingService.process(
                    payment
            );
        }
    }

    private void processOperationalExpenses(
            StockOnboardingRequest req
    ) {

        if (req.getOperationalExpenses() == null) {
            return;
        }

        int index = 0;

        for (var expense : req.getOperationalExpenses()) {

            CreateOperationalExpenseRequest create =
                    new CreateOperationalExpenseRequest();

            create.setBranchId(
                    req.getBranchId()
            );

            create.setExpenseAccountId(
                    expense.getExpenseAccountId()
            );

            create.setFundingAccountId(
                    req.getFundingAccountId()
            );

            create.setDescription(
                    expense.getDescription()
            );

            create.setAmount(
                    expense.getAmount()
            );

            create.setAccountingDate(
                    req.getAccountingDate()
            );

            create.setAutoPay(
                    Boolean.TRUE.equals(
                            req.getAutoPayOperationalExpenses()
                    )
            );

            create.setReference(
                    req.getReference()
                            + "::EXPENSE::"
                            + index
            );

            create.setSourceModule(
                    "STOCK_ONBOARDING"
            );

            create.setSourceId(
                    UUID.nameUUIDFromBytes(
                            (
                                    req.getReference()
                                            + "::EXPENSE::"
                                            + index
                            ).getBytes()
                    )
            );

            operationalExpenseService.createExpense(
                    create
            );

            index++;
        }
    }

    private String normalize(String s) {
        return s.trim().toLowerCase();
    }

    private void validateRequest(
            StockOnboardingRequest req
    ) {

        if (req.getVariantId() == null) {

            if (req.getClassification() == null
                    || req.getClassification().isBlank()) {

                throw new IllegalArgumentException(
                        "classification is required"
                );
            }
        }

        if (req.getBranchId() == null) {
            throw new IllegalArgumentException(
                    "branchId is required"
            );
        }

        if (req.getSuppliers() == null
                || req.getSuppliers().isEmpty()) {

            throw new IllegalArgumentException(
                    "At least one supplier is required"
            );
        }


        if (req.getPricing() == null
                || req.getPricing().isEmpty()) {

            throw new IllegalArgumentException(
                    "Pricing is required"
            );
        }

        if (req.getAccountingDate() == null) {
            throw new IllegalArgumentException(
                    "accountingDate is required"
            );
        }

        if (
                Boolean.TRUE.equals(
                        req.getAutoPayOperationalExpenses()
                )
        ) {
            if (req.getFundingAccountId() == null) {
                throw new IllegalArgumentException(
                        "fundingAccountId is required when autoPayOperationalExpenses=true"
                );
            }
        }

        Set<String> packagingNames = new HashSet<>();
        Set<Long> packagingUnits = new HashSet<>();

        long basePackagingCount = 0;

        if (req.getPackagings() != null) {

            for (var p : req.getPackagings()) {

                if (p.getName() == null || p.getName().isBlank()) {
                    throw new IllegalArgumentException(
                            "Packaging name is required"
                    );
                }

                String normalized = normalize(p.getName());

                if (!packagingNames.add(normalized)) {
                    throw new IllegalArgumentException(
                            "Duplicate packaging name: " + p.getName()
                    );
                }

                if (p.getUnits() == null || p.getUnits() <= 0) {
                    throw new IllegalArgumentException(
                            "Packaging units must be > 0"
                    );
                }

                // NEW: units uniqueness validation
                if (!packagingUnits.add(p.getUnits())) {
                    throw new IllegalArgumentException(
                            "Duplicate packaging units detected: "
                                    + p.getUnits()
                    );
                }

                // NEW: exactly one base packaging
                if (p.getUnits() == 1L) {
                    basePackagingCount++;
                }
            }

            // NEW: enforce exactly one base packaging
            if (basePackagingCount == 0) {
                throw new IllegalArgumentException(
                        "Exactly one packaging with units=1 is required"
                );
            }

            if (basePackagingCount > 1) {
                throw new IllegalArgumentException(
                        "Multiple base packagings detected (units=1)"
                );
            }
        }

        Set<String> pricingPackagings =
                new HashSet<>();

        for (var p : req.getPricing()) {

            if (p.getPackagingName() == null
                    || p.getPackagingName().isBlank()) {

                throw new IllegalArgumentException(
                        "Pricing packagingName required"
                );
            }

            String normalized =
                    normalize(p.getPackagingName());

            if (!pricingPackagings.add(normalized)) {

                throw new IllegalArgumentException(
                        "Duplicate pricing packaging: "
                                + p.getPackagingName()
                );
            }

            if (p.getSellingPrice() == null
                    || p.getSellingPrice()
                    .compareTo(BigDecimal.ZERO) <= 0) {

                throw new IllegalArgumentException(
                        "sellingPrice must be > 0"
                );
            }
        }

        Set<String> supplierReceiptKeys =
                new HashSet<>();

        if (Boolean.TRUE.equals(req.getAutoPaySuppliers())) {

            if (req.getFundingAccountId() == null) {
                throw new IllegalArgumentException(
                        "fundingAccountId is required when autoPaySuppliers=true"
                );
            }

            if (req.getSupplierPaymentMethod() == null) {
                throw new IllegalArgumentException(
                        "supplierPaymentMethod is required when autoPaySuppliers=true"
                );
            }
        }

        if (req.getOperationalExpenses() != null) {

            for (var expense : req.getOperationalExpenses()) {

                if (expense.getDescription() == null
                        || expense.getDescription().isBlank()) {
                    throw new IllegalArgumentException(
                            "Operational expense description is required"
                    );
                }

                if (expense.getAmount() == null
                        || expense.getAmount().compareTo(BigDecimal.ZERO) <= 0) {
                    throw new IllegalArgumentException(
                            "Operational expense amount must be > 0"
                    );
                }

                if (expense.getExpenseAccountId() == null) {
                    throw new IllegalArgumentException(
                            "expenseAccountId is required"
                    );
                }
            }
        }

        for (var s : req.getSuppliers()) {

            if (s.getSupplierId() == null
                    && (s.getSupplierName() == null
                    || s.getSupplierName().isBlank())) {

                throw new IllegalArgumentException(
                        "supplierId or supplierName is required"
                );
            }

            if (s.getPackagingName() == null
                    || s.getPackagingName().isBlank()) {

                throw new IllegalArgumentException(
                        "Supplier packagingName required"
                );
            }

            String normalizedPackaging =
                    normalize(s.getPackagingName());

            boolean exists =
                    packagingNames.contains(normalizedPackaging)
                            || normalizedPackaging.equals("piece");

            if (!exists) {
                throw new IllegalArgumentException(
                        "Unknown packaging referenced by supplier: "
                                + s.getPackagingName()
                );
            }

            if (s.getUnitsSupplied() == null
                    || s.getUnitsSupplied() <= 0) {

                throw new IllegalArgumentException(
                        "unitsSupplied must be > 0"
                );
            }

            if (s.getUnitCost() == null
                    || s.getUnitCost().compareTo(BigDecimal.ZERO) <= 0) {

                throw new IllegalArgumentException(
                        "unitCost must be > 0"
                );
            }

            String receiptKey =
                    normalize(
                            s.getSupplierName() != null
                                    ? s.getSupplierName()
                                    : String.valueOf(s.getSupplierId())
                    )
                            + "::"
                            + normalize(s.getPackagingName())
                            + "::"
                            + s.getUnitsSupplied()
                            + "::"
                            + s.getUnitCost()
                            .stripTrailingZeros()
                            .toPlainString();

            if (!supplierReceiptKeys.add(receiptKey)) {
                throw new IllegalArgumentException(
                        "Duplicate supplier receipt line detected"
                );
            }
        }
    }

    private void validateFundingAccountPaymentMethod(
            StockOnboardingRequest req
    ) {
        if (!Boolean.TRUE.equals(req.getAutoPaySuppliers()) && !Boolean.TRUE.equals(req.getAutoPayOperationalExpenses())) {
            return;
        }

        Account fundingAccount =
                accountRepository
                        .findByTenantIdAndBranchIdAndId(
                                tenantId(),
                                req.getBranchId(),
                                req.getFundingAccountId()
                        )
                        .orElseThrow(() ->
                                new IllegalArgumentException(
                                        "Funding account not found"
                                )
                        );

        String role =
                fundingAccount.getRole();

        boolean valid =
                ("CASH".equals(role)
                        && req.getSupplierPaymentMethod()
                        == SupplierPaymentMethod.CASH)
                        ||
                        ("BANK".equals(role)
                                && req.getSupplierPaymentMethod()
                                == SupplierPaymentMethod.BANK)
                        ||
                        ("MPESA".equals(role)
                                && req.getSupplierPaymentMethod()
                                == SupplierPaymentMethod.MPESA);

        if (!valid) {
            throw new IllegalArgumentException(
                    "Funding account role and supplier payment method do not match"
            );
        }
    }
}