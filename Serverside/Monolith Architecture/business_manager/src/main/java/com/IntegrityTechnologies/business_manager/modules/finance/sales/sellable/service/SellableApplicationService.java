package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.service;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.domain.ResolutionMode;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.domain.SellableContext;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.domain.SellableSnapshot;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto.SellableResolveRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto.SellableResolveResponse;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class SellableApplicationService {

    private final SellableResolutionService resolutionService;

    public SellableResolveResponse resolve(SellableResolveRequest req) {

        SellableSnapshot snap = resolutionService.resolve(
                SellableContext.builder()
                        .tenantId(TenantContext.getTenantId())
                        .branchId(req.getBranchId())
                        .productVariantId(req.getProductVariantId())
                        .packagingId(req.getPackagingId())
                        .quantity(req.getQuantity() != null ? req.getQuantity() : 1L)
                        .customerId(req.getCustomerId())
                        .customerGroupId(req.getCustomerGroupId())
                        .batchIds(req.getBatchIds())
                        .mode(ResolutionMode.UI_FAST)
                        .build()
        );

        return SellableResolveResponse.builder()
                .productVariantId(snap.getProductVariantId())
                .packagingId(snap.getPackagingId())
                .requestedQuantity(snap.getQuantity())
                .baseUnits(snap.getBaseUnits())
                .unitPrice(snap.getUnitPrice())
                .totalPrice(snap.getTotalPrice())
                .availableStock(snap.getAvailableStock())
                .totalCost(snap.getTotalCost())
                .batchAllocations(snap.getAllocations())
                .adjustments(snap.getAdjustments())
                .build();
    }
}