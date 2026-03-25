package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.mapper;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.ProductVariantDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariant;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import java.math.BigDecimal;

@Mapper(componentModel = "spring")
public interface ProductVariantMapper {

    @Mapping(target = "productId", source = "product.id")
    @Mapping(target = "productName", source = "product.name")
    @Mapping(target = "barcode", source = "barcode") // ✅ explicit
    @Mapping(target = "barcodeImagePath", source = "barcodeImagePath") // ✅ explicit
    @Mapping(target = "minimumPercentageProfit", source = "minimumPercentageProfit")
    @Mapping(target = "minimumProfit", source = "minimumProfit")
    @Mapping(target = "imageUrls", ignore = true)
    ProductVariantDTO toDTO(ProductVariant entity);
}