package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.mapper;

import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductImageDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductImage;
import org.mapstruct.Mapper;

import java.util.Collection;
import java.util.List;

@Mapper(componentModel = "spring")
public interface ProductImageMapper {

    ProductImageDTO toDto(
            ProductImage image
    );

    List<ProductImageDTO> toDtos(
            Collection<ProductImage> images
    );
}