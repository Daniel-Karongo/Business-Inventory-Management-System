package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.service;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.CustomerPrice;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingAdjustment;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingContext;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingResult;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.repository.CustomerPriceRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.repository.PricingRule;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.List;

@Component
@RequiredArgsConstructor
public class CustomerPricingRule implements PricingRule {

    private final CustomerPriceRepository repo;

    @Override
    public boolean applies(PricingContext ctx) {
        return ctx.getCustomerId() != null || ctx.getCustomerGroupId() != null;
    }

    @Override
    public PricingResult apply(PricingContext ctx, PricingResult current) {

        BigDecimal cost = ctx.getCost();

        if (cost == null || cost.compareTo(BigDecimal.ZERO) <= 0) {
            return current;
        }

        List<CustomerPrice> matches = repo.findBestMatch(
                ctx.getProductVariantId(),
                ctx.getPackagingId(),
                ctx.getTenantId(),
                ctx.getBranchId(),
                ctx.getCustomerId(),
                ctx.getCustomerGroupId(),
                ctx.getQuantity(),
                ctx.getPricingTime()
        );

        if (!matches.isEmpty()) {
            CustomerPrice cp = matches.get(0);

            BigDecimal old = current.getFinalPrice();

            current.setFinalPrice(cp.getPrice());

            current.getAdjustments().add(
                    new PricingAdjustment(
                            "CUSTOMER_PRICE",
                            cp.getId().toString(),
                            cp.getPrice().subtract(old),
                            "Customer pricing applied"
                    )
            );
        }

        return current;
    }

    @Override
    public int priority() {
        return 90;
    }
}