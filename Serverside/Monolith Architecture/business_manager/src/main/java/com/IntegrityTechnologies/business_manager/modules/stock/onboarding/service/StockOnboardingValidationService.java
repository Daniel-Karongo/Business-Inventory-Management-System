package com.IntegrityTechnologies.business_manager.modules.stock.onboarding.service;

import com.IntegrityTechnologies.business_manager.config.bulk.BulkError;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import com.IntegrityTechnologies.business_manager.modules.stock.onboarding.dto.StockOnboardingRequest;
import com.IntegrityTechnologies.business_manager.modules.stock.onboarding.dto.StockOnboardingRowValidationResult;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.repository.ProductVariantRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;

@Service
@RequiredArgsConstructor
public class StockOnboardingValidationService {

    private final InventoryService inventoryService;

    private final ProductRepository productRepository;

    private final ProductVariantRepository variantRepo;

    public StockOnboardingRowValidationResult validate(
            int rowNumber,
            StockOnboardingRequest req
    ) {

        List<BulkError> errors =
                new ArrayList<>();

        /*
         * =====================================================
         * PACKAGING MAP
         * =====================================================
         */

        Map<String, Long> packagingUnits =
                new HashMap<>();

        if (req.getPackagings() != null) {

            for (var p : req.getPackagings()) {

                if (p.getName() == null || p.getUnits() == null) {
                    continue;
                }

                packagingUnits.put(
                        normalize(p.getName()),
                        p.getUnits()
                );
            }
        }

        /*
         * =====================================================
         * RESOLVE EXISTING VARIANT
         * =====================================================
         */

        UUID variantId =
                resolveExistingVariantId(req);

        /*
         * =====================================================
         * EXISTING AVG COST
         * =====================================================
         */

        BigDecimal avgCost = null;

        if (variantId != null) {

            avgCost =
                    inventoryService.getAverageCost(
                            variantId,
                            req.getBranchId()
                    );
        }

        /*
         * =====================================================
         * SELLING PRICE VALIDATION
         * =====================================================
         */

        if (req.getPricing() != null) {

            for (int i = 0; i < req.getPricing().size(); i++) {

                var p =
                        req.getPricing().get(i);

                if (
                        p.getPackagingName() == null
                                || p.getSellingPrice() == null
                ) {
                    continue;
                }

                String key =
                        normalize(p.getPackagingName());

                Long units =
                        packagingUnits.get(key);

                if (units == null || units <= 0) {
                    continue;
                }

                /*
                 * =====================================================
                 * NORMALIZE SELLING PRICE TO BASE UNIT
                 * =====================================================
                 */

                BigDecimal normalizedSellingPrice =
                        p.getSellingPrice()
                                .divide(
                                        BigDecimal.valueOf(units),
                                        6,
                                        RoundingMode.HALF_UP
                                );

                /*
                 * =====================================================
                 * VALIDATE AGAINST EXISTING AVG COST
                 * =====================================================
                 */

                if (
                        avgCost != null
                                && avgCost.compareTo(BigDecimal.ZERO) > 0
                                && normalizedSellingPrice.compareTo(avgCost) < 0
                ) {

                    errors.add(
                            BulkError.builder()
                                    .row(rowNumber)
                                    .field(
                                            "pricing[" + i + "].sellingPrice"
                                    )
                                    .code(
                                            "SELLING_PRICE_BELOW_COST"
                                    )
                                    .message(
                                            "Selling price "
                                                    + p.getSellingPrice()
                                                    + " for packaging '"
                                                    + p.getPackagingName()
                                                    + "' is below existing average unit cost "
                                                    + avgCost
                                    )
                                    .rejectedValue(
                                            p.getSellingPrice()
                                    )
                                    .build()
                    );
                }
            }
        }

        return StockOnboardingRowValidationResult
                .builder()
                .row(rowNumber)
                .valid(errors.isEmpty())
                .errors(errors)
                .build();
    }

    /*
     * =====================================================
     * EXISTING VARIANT RESOLUTION
     * =====================================================
     */

    private UUID resolveExistingVariantId(
            StockOnboardingRequest req
    ) {

        /*
         * =====================================================
         * DIRECT VARIANT
         * =====================================================
         */

        if (req.getVariantId() != null) {
            return req.getVariantId();
        }

        /*
         * =====================================================
         * CANNOT RESOLVE
         * =====================================================
         */

        if (
                req.getProductName() == null
                        || req.getProductName().isBlank()
                        || req.getClassification() == null
                        || req.getClassification().isBlank()
        ) {
            return null;
        }

        /*
         * =====================================================
         * FIND PRODUCT
         * =====================================================
         */

        Product product =
                productRepository
                        .findByTenantIdAndBranchIdAndNameIgnoreCase(
                                TenantContext.getTenantId(),
                                req.getBranchId(),
                                req.getProductName().trim()
                        )
                        .orElse(null);

        if (product == null) {
            return null;
        }

        /*
         * =====================================================
         * FIND VARIANT
         * =====================================================
         */

        Optional<ProductVariant> variant =
                variantRepo
                        .findByTenantIdAndBranchIdAndProduct_IdAndClassification(
                                TenantContext.getTenantId(),
                                req.getBranchId(),
                                product.getId(),
                                req.getClassification()
                        );

        return variant
                .map(ProductVariant::getId)
                .orElse(null);
    }

    /*
     * =====================================================
     * NORMALIZATION
     * =====================================================
     */

    private String normalize(
            String s
    ) {

        return s.trim().toLowerCase();
    }
}