package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.service;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingAdjustment;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingContext;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingResult;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.ProductPrice;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.repository.PricingRule;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.Comparator;
import java.util.List;

@Service
@RequiredArgsConstructor
public class PricingEngineService {

    private final ProductPriceService basePriceService;
    private final List<PricingRule> rules;

    public PricingResult resolve(PricingContext ctx) {

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
        // 🔒 MIN PRICE ENFORCEMENT (NEW)
        // =====================================================
        if (ctx.getPolicy() != null && ctx.getPolicy().isEnforceMinimumPrice()) {

            BigDecimal min = base.getPrice();

            if (result.getFinalPrice().compareTo(min) < 0) {
                throw new IllegalStateException(
                        "Price below allowed minimum: " + result.getFinalPrice()
                );
            }
        }

        if (result.getFinalPrice().compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalStateException("Final price cannot be negative");
        }

        return result;
    }
}