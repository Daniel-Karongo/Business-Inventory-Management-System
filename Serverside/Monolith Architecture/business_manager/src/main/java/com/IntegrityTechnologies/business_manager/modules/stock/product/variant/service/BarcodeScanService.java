package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.service;

import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryItem;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryItemRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.dto.BarcodeScanResponse;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class BarcodeScanService {

    private final ProductVariantRepository variantRepo;
    private final InventoryItemRepository inventoryRepo;

    @Transactional(readOnly = true)
    public BarcodeScanResponse scan(String barcode, UUID branchId) {

        ProductVariant variant = variantRepo.findByBarcode(barcode)
                .or(() -> variantRepo.findBySku(barcode))
                .orElseThrow(() ->
                        new EntityNotFoundException("Invalid barcode or SKU")
                );

        InventoryItem inventory = null;

        if (branchId != null) {
            inventory = inventoryRepo
                    .findByProductVariant_IdAndBranchId(
                            variant.getId(), branchId
                    )
                    .orElse(null);
        }

        return BarcodeScanResponse.builder()
                .productId(variant.getProduct().getId())
                .productName(variant.getProduct().getName())

                .variantId(variant.getId())
                .classification(variant.getClassification())
                .sku(variant.getSku())
                .barcode(variant.getBarcode())

                .sellingPrice(
                        variant.getMinimumSellingPrice()
                )

                .branchId(branchId)
                .quantityOnHand(
                        inventory != null
                                ? inventory.getQuantityOnHand()
                                : null
                )
                .build();
    }
}