package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.service;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingAdjustment;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingContext;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingResult;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.ProductPrice;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.repository.PricingRule;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.Comparator;
import java.util.List;

@Service
@RequiredArgsConstructor
public class PricingEngineService {

    /**
     * SINGLE SOURCE OF TRUTH FOR PRICING
     *
     * ALL pricing must go through this service.
     *
     * Forbidden:
     * - Using ProductPriceRepository directly
     * - Using minimumSellingPrice for final price
     * - Using InventoryBatch.unitSellingPrice
     */

    private final ProductPriceService basePriceService;
    private final List<PricingRule> rules;
    private final ProductVariantRepository variantRepository;

    public PricingResult resolve(PricingContext ctx) {

        ProductVariant variant = variantRepository
                .findById(ctx.getProductVariantId())
                .orElseThrow(() -> new IllegalArgumentException("Variant not found"));

        ProductPrice base = basePriceService.resolvePrice(
                ctx.getProductVariantId(),
                ctx.getPackagingId(),
                ctx.getQuantity()
        );

        PricingResult result = new PricingResult();
        result.setBasePrice(base.getPrice());
        result.setFinalPrice(base.getPrice());
        result.setResolvedPriceId(base.getId());

        result.getAdjustments().add(
                new PricingAdjustment("BASE", "PRODUCT_PRICE", base.getPrice(), "Base price")
        );

        // =====================================================
        // APPLY RULES
        // =====================================================
        rules.stream()
                .sorted(Comparator.comparingInt(PricingRule::priority).reversed())
                .filter(r -> r.applies(ctx))
                .forEach(r -> r.apply(ctx, result));

        // =====================================================
        // 🔒 MIN PRICE ENFORCEMENT (FIXED)
        // =====================================================

        BigDecimal minPrice = variant.getMinimumSellingPrice();

        if (ctx.getPolicy() != null &&
                ctx.getPolicy().isEnforceMinimumPrice() &&
                minPrice != null) {

            if (result.getFinalPrice().compareTo(minPrice) < 0) {

                BigDecimal old = result.getFinalPrice();

                result.setFinalPrice(minPrice);

                result.getAdjustments().add(
                        new PricingAdjustment(
                                "MIN_PRICE_ENFORCED",
                                "SYSTEM",
                                minPrice.subtract(old),
                                "Adjusted to minimum selling price"
                        )
                );
            }
        }

        if (result.getFinalPrice().compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalStateException("Final price cannot be negative");
        }

        return result;
    }
}