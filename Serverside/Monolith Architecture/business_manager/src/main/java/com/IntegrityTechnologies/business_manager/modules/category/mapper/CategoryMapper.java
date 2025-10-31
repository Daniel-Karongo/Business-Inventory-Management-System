package com.IntegrityTechnologies.business_manager.modules.category.mapper;

import com.IntegrityTechnologies.business_manager.modules.category.dto.CategoryDTO;
import com.IntegrityTechnologies.business_manager.modules.category.model.Category;
import org.mapstruct.*;

@Mapper(componentModel = "spring")
public interface CategoryMapper {

    CategoryDTO toDTO(Category category);

    Category toEntity(CategoryDTO dto);

    @BeanMapping(nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE)
    void updateEntityFromDTO(CategoryDTO dto, @MappingTarget Category entity);
}