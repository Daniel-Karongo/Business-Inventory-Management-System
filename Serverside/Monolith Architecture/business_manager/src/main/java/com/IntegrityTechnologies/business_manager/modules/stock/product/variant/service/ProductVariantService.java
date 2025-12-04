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

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class ProductVariantService {

    private final ProductVariantRepository variantRepo;
    private final ProductRepository productRepo;
    private final ProductVariantMapper mapper;

    public ProductVariantDTO createVariant(ProductVariantCreateDTO dto) {
        Product product = productRepo.findById(dto.getProductId())
                .orElseThrow(() -> new EntityNotFoundException("Product not found"));

        if (variantRepo.existsByProduct_IdAndClassification(dto.getProductId(), dto.getClassification())) {
            throw new IllegalArgumentException("Variant with classification already exists");
        }

        ProductVariant v = new ProductVariant();
        v.setProduct(product);
        v.setClassification(dto.getClassification());
        // Auto-generate SKU if missing
        v.setSku(dto.getSku() != null ? dto.getSku() : product.getSku() + "-" + dto.getClassification());

        return mapper.toDTO(variantRepo.save(v));
    }

    public BigDecimal computeMinSelling(BigDecimal buying, Double pct) {
        if (buying == null) buying = BigDecimal.ZERO;
        if (pct == null) pct = 0D;

        double multiplier = 1 + pct / 100.0;
        return buying.multiply(BigDecimal.valueOf(multiplier));
    }

    public ProductVariantDTO getVariant(UUID id) {
        return mapper.toDTO(
                variantRepo.findById(id)
                        .orElseThrow(() -> new EntityNotFoundException("Variant not found"))
        );
    }

    public List<ProductVariantDTO> getVariantsForProduct(UUID productId) {
        return variantRepo.findByProduct_Id(productId)
                .stream()
                .map(mapper::toDTO)
                .toList();
    }

    public ProductVariantDTO updateVariant(UUID id, ProductVariantUpdateDTO dto) {
        ProductVariant v = variantRepo.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Variant not found"));

        if (dto.getClassification() != null) v.setClassification(dto.getClassification());
        if (dto.getAverageBuyingPrice() != null) v.setAverageBuyingPrice(dto.getAverageBuyingPrice());
        if (dto.getSku() != null) v.setSku(dto.getSku());

        // AUTO RECALCULATE USING PRODUCT PERCENTAGE
        Double pct = v.getProduct().getMinimumPercentageProfit();
        v.setMinimumSellingPrice(
                computeMinSelling(v.getAverageBuyingPrice(), pct)
        );

        return mapper.toDTO(variantRepo.save(v));
    }

    public void deleteVariant(UUID id) {
        ProductVariant v = variantRepo.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Variant not found"));
        variantRepo.delete(v);
    }
}