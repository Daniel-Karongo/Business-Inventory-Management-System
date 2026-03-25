package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.service;

import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.repository.ProductVariantRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingAdjustment;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingContext;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingResult;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.repository.PricingRule;
import lombok.AllArgsConstructor;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;

@Component
@RequiredArgsConstructor
public class MinimumProfitRule implements PricingRule {

    private final ProductVariantRepository variantRepository;

    @Override
    public boolean applies(PricingContext ctx) {
        return ctx.getPolicy() != null && ctx.getPolicy().isEnforceMinimumPrice();
    }

    @Override
    public PricingResult apply(PricingContext ctx, PricingResult current) {

        ProductVariant variant = variantRepository.findById(ctx.getProductVariantId())
                .orElseThrow(() -> new IllegalArgumentException("Variant not found"));

        BigDecimal cost = ctx.getCost();
        MinimumConfig cfg = resolveMinimum(variant);

        if (cfg == null || cost == null) return current;

        BigDecimal min;

        if (cfg.isPercentage) {
            min = cost.add(cost.multiply(cfg.value));
        } else {
            min = cost.add(cfg.value);
        }

        if (current.getFinalPrice().compareTo(min) < 0) {

            BigDecimal old = current.getFinalPrice();

            current.setFinalPrice(min);

            current.getAdjustments().add(
                    new PricingAdjustment(
                            "MIN_PROFIT",
                            "RULE",
                            min.subtract(old),
                            "Adjusted to minimum profit threshold"
                    )
            );
        }

        return current;
    }

    private MinimumConfig resolveMinimum(ProductVariant variant) {

        if (variant.getMinimumProfit() != null) {
            return new MinimumConfig(variant.getMinimumProfit(), false);
        }

        if (variant.getMinimumPercentageProfit() != null &&
                variant.getMinimumPercentageProfit() != 0D) {
            return new MinimumConfig(
                    BigDecimal.valueOf(variant.getMinimumPercentageProfit()),
                    true
            );
        }

        Product product = variant.getProduct();

        if (product.getMinimumProfit() != null) {
            return new MinimumConfig(product.getMinimumProfit(), false);
        }

        if (product.getMinimumPercentageProfit() != null &&
                product.getMinimumPercentageProfit() != 0D) {
            return new MinimumConfig(
                    BigDecimal.valueOf(product.getMinimumPercentageProfit()),
                    true
            );
        }

        Category category = product.getCategory();

        if (category != null) {
            if (category.getMinimumProfit() != null) {
                return new MinimumConfig(category.getMinimumProfit(), false);
            }

            if (category.getMinimumPercentageProfit() != null &&
                    category.getMinimumPercentageProfit() != 0D) {
                return new MinimumConfig(
                        BigDecimal.valueOf(category.getMinimumPercentageProfit()),
                        true
                );
            }
        }

        return null;
    }

    @Override
    public int priority() {
        return 100; // highest priority
    }
}

@AllArgsConstructor
class MinimumConfig {
    BigDecimal value;
    boolean isPercentage;
}