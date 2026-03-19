package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.service;

import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryItemRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.mapper.ProductVariantMapper;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchContext;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class ProductVariantService {

    private final ProductVariantRepository variantRepo;
    private final ProductRepository productRepo;
    private final ProductVariantMapper mapper;
    private final VariantBarcodeService barcodeService;
    private final InventoryItemRepository inventoryItemRepository;

    private UUID tenantId() { return TenantContext.getTenantId(); }
    private UUID branchId() { return BranchContext.get(); }

    @Transactional
    public ProductVariantDTO createVariant(ProductVariantCreateDTO dto) {

        Product product = productRepo.findByIdAndTenantIdAndBranchIdAndDeletedFalse(
                dto.getProductId(),
                tenantId(),
                branchId()
        ).orElseThrow(() -> new EntityNotFoundException("Product not found"));

        if (variantRepo.existsByTenantIdAndBranchIdAndProduct_IdAndClassification(
                tenantId(),
                branchId(),
                dto.getProductId(),
                dto.getClassification()
        )) {
            throw new IllegalArgumentException("Variant already exists");
        }

        String sku =
                dto.getSku() != null
                        ? dto.getSku()
                        : product.getSku() + "-" + normalize(dto.getClassification());

        if (variantRepo.existsByTenantIdAndBranchIdAndSku(
                tenantId(),
                branchId(),
                sku
        )) {
            throw new IllegalArgumentException("SKU already exists: " + sku);
        }

        ProductVariant v = new ProductVariant();
        v.setProduct(product);
        v.setClassification(dto.getClassification());
        v.setSku(sku);
        v.setTenantId(tenantId());
        v.setBranchId(branchId());

        v = variantRepo.save(v);

        return barcodeService.generateBarcodeIfMissing(v.getId());
    }

    public ProductVariant getEntity(UUID id) {
        return variantRepo.findByIdSafe(
                id,
                false,
                tenantId(),
                branchId()
        ).orElseThrow(() -> new EntityNotFoundException("Variant not found"));
    }

    public List<ProductVariant> getEntitiesForProduct(UUID productId) {
        return variantRepo.findByProduct_IdSafe(
                productId,
                false,
                tenantId(),
                branchId()
        );
    }

    public ProductVariantDTO getVariant(UUID id) {
        return mapper.toDTO(getEntity(id));
    }

    public List<ProductVariantDTO> getVariantsForProduct(UUID productId) {
        return getEntitiesForProduct(productId)
                .stream()
                .map(mapper::toDTO)
                .toList();
    }

    @Transactional
    public ProductVariantDTO updateVariant(UUID id, ProductVariantUpdateDTO dto) {

        ProductVariant v = getEntity(id);

        if (dto.getClassification() != null &&
                !dto.getClassification().equals(v.getClassification())) {

            if (variantRepo.existsByTenantIdAndBranchIdAndProduct_IdAndClassification(
                    tenantId(),
                    branchId(),
                    v.getProduct().getId(),
                    dto.getClassification()
            )) {
                throw new IllegalArgumentException("Variant classification already exists");
            }

            v.setClassification(dto.getClassification());
        }

        if (dto.getAverageBuyingPrice() != null)
            v.setAverageBuyingPrice(dto.getAverageBuyingPrice());

        if (dto.getSku() != null && !dto.getSku().equals(v.getSku())) {

            if (variantRepo.existsByTenantIdAndBranchIdAndSku(
                    tenantId(),
                    branchId(),
                    dto.getSku()
            )) {
                throw new IllegalArgumentException("SKU already exists: " + dto.getSku());
            }

            v.setSku(dto.getSku());
        }

        Double pct = v.getProduct().getMinimumPercentageProfit();

        v.setMinimumSellingPrice(
                computeMinSelling(v.getAverageBuyingPrice(), pct)
        );

        return mapper.toDTO(variantRepo.save(v));
    }

    @Transactional
    public void deleteVariant(UUID variantId) {

        ProductVariant variant = getEntity(variantId);

        UUID productId = variant.getProduct().getId();

        boolean hasInventory =
                inventoryItemRepository.existsByProductVariantIdAndTenantIdAndBranchIdAndDeletedFalse(
                        variantId,
                        tenantId(),
                        branchId()
                );

        if (hasInventory) {
            throw new IllegalStateException(
                    "Cannot delete variant '" +
                            variant.getClassification() +
                            "' because inventory exists."
            );
        }

        long activeVariants =
                variantRepo.countByTenantIdAndBranchIdAndProduct_IdAndDeletedFalse(
                        tenantId(),
                        branchId(),
                        productId
                );

        if (activeVariants <= 1) {
            throw new IllegalStateException(
                    "Cannot delete the last remaining variant."
            );
        }

        variant.setDeleted(true);
        variantRepo.save(variant);
    }

    public BigDecimal computeMinSelling(BigDecimal buying, Double pct) {

        if (buying == null) buying = BigDecimal.ZERO;
        if (pct == null) pct = 0D;

        BigDecimal percentage =
                BigDecimal.valueOf(pct)
                        .divide(BigDecimal.valueOf(100), 6, RoundingMode.HALF_UP);

        return buying.multiply(BigDecimal.ONE.add(percentage))
                .setScale(2, RoundingMode.HALF_UP);
    }

    private String normalize(String input) {
        return input == null ? "" :
                input.replaceAll("[^A-Za-z0-9]", "")
                        .toUpperCase();
    }
}