package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.service;

import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.mapper.ProductVariantMapper;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class ProductVariantService {

    private final ProductVariantRepository variantRepo;
    private final ProductRepository productRepo;
    private final ProductVariantMapper mapper;
    private final VariantBarcodeService barcodeService;

    @Transactional
    public ProductVariantDTO createVariant(ProductVariantCreateDTO dto) {

        Product product = productRepo.findById(dto.getProductId())
                .orElseThrow(() -> new EntityNotFoundException("Product not found"));

        if (variantRepo.existsByProduct_IdAndClassification(
                dto.getProductId(), dto.getClassification())) {
            throw new IllegalArgumentException("Variant already exists");
        }

        ProductVariant v = new ProductVariant();
        v.setProduct(product);
        v.setClassification(dto.getClassification());
        v.setSku(
                dto.getSku() != null
                        ? dto.getSku()
                        : product.getSku() + "-" + dto.getClassification()
        );

        v = variantRepo.save(v);

        // 🔥 FORCE barcode creation and return UPDATED entity
        return barcodeService.generateBarcodeIfMissing(v.getId());
    }

    /* =============================
       CORE HELPERS (REQUIRED)
       ============================= */

    public ProductVariant getEntity(UUID id) {
        return variantRepo.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Variant not found"));
    }

    public List<ProductVariant> getEntitiesForProduct(UUID productId) {
        return variantRepo.findByProduct_Id(productId);
    }

    /* =============================
       READ
       ============================= */

    public ProductVariantDTO getVariant(UUID id) {
        return mapper.toDTO(getEntity(id));
    }

    public List<ProductVariantDTO> getVariantsForProduct(UUID productId) {
        return getEntitiesForProduct(productId)
                .stream()
                .map(mapper::toDTO)
                .toList();
    }

    /* =============================
       UPDATE
       ============================= */

    public ProductVariantDTO updateVariant(UUID id, ProductVariantUpdateDTO dto) {

        ProductVariant v = getEntity(id);

        if (dto.getClassification() != null)
            v.setClassification(dto.getClassification());

        if (dto.getAverageBuyingPrice() != null)
            v.setAverageBuyingPrice(dto.getAverageBuyingPrice());

        if (dto.getSku() != null)
            v.setSku(dto.getSku());

        Double pct = v.getProduct().getMinimumPercentageProfit();
        v.setMinimumSellingPrice(
                computeMinSelling(v.getAverageBuyingPrice(), pct)
        );

        return mapper.toDTO(variantRepo.save(v));
    }

    /* =============================
       DELETE
       ============================= */

    public void deleteVariant(UUID id) {
        variantRepo.delete(getEntity(id));
    }

    /* =============================
       PRICING
       ============================= */

    public BigDecimal computeMinSelling(BigDecimal buying, Double pct) {
        if (buying == null) buying = BigDecimal.ZERO;
        if (pct == null) pct = 0D;
        return buying.multiply(BigDecimal.valueOf(1 + pct));
    }
}