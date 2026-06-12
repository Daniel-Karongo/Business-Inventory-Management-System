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
    @Mapping(target = "barcode", source = "barcode")
    @Mapping(target = "barcodeImagePath", source = "barcodeImagePath")
    @Mapping(target = "minimumPercentageProfit", source = "minimumPercentageProfit")
    @Mapping(target = "minimumProfit", source = "minimumProfit")
    @Mapping(target = "deleted", source = "deleted")
    @Mapping(target = "imageUrls", ignore = true)
    @Mapping(target = "thumbnailFileName", ignore = true)
    @Mapping(target = "primaryImageFileName", ignore = true)
    ProductVariantDTO toDTO(ProductVariant entity);
}