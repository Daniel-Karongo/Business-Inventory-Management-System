package com.IntegrityTechnologies.business_manager.modules.finance.sales.base.service;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.dto.*;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.domain.*;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.service.SellableResolutionService;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class SalePreviewService {

    private final SellableResolutionService resolutionService;

    public SaleLinePreviewResponse preview(SaleLinePreviewRequest req) {

        SellableSnapshot snap = resolutionService.resolve(
                SellableContext.builder()
                        .tenantId(TenantContext.getTenantId())
                        .branchId(req.getBranchId())
                        .productVariantId(req.getProductVariantId())
                        .packagingId(req.getPackagingId())
                        .quantity(req.getQuantity())
                        .customerId(req.getCustomerId())
                        .customerGroupId(req.getCustomerGroupId())
                        .batchIds(
                                req.getBatchSelections() == null ? null :
                                        req.getBatchSelections().stream()
                                                .map(BatchSelectionDto::getBatchId)
                                                .toList()
                        )
                        .mode(ResolutionMode.PREVIEW)
                        .build()
        );

        return SaleLinePreviewResponse.builder()
                .productVariantId(snap.getProductVariantId())
                .packagingId(snap.getPackagingId())
                .requestedQuantity(snap.getQuantity())
                .baseUnits(snap.getBaseUnits())
                .unitPrice(snap.getUnitPrice())
                .totalPrice(snap.getTotalPrice())
                .totalCost(snap.getTotalCost())
                .availableStock(snap.getAvailableStock())
                .batchAllocations(snap.getAllocations())
                .adjustments(snap.getAdjustments())
                .build();
    }
}