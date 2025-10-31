package com.IntegrityTechnologies.business_manager.modules.product.mapper;

import com.IntegrityTechnologies.business_manager.modules.product.dto.ProductDTO;
import com.IntegrityTechnologies.business_manager.modules.product.model.Product;
import org.mapstruct.*;

@Mapper(componentModel = "spring")
public interface ProductMapper {

    // map Category.id -> categoryId in DTO
    @Mapping(source = "category.id", target = "categoryId")
    ProductDTO toDTO(Product product);

    // map categoryId back to Category.id for updateEntityFromDTO to use (MapStruct will create a Category with id)
    @Mapping(source = "categoryId", target = "category.id")
    Product toEntity(ProductDTO dto);

    @BeanMapping(nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE)
    @Mapping(target = "id", ignore = true)
    void updateEntityFromDTO(ProductDTO dto, @MappingTarget Product entity);
}
