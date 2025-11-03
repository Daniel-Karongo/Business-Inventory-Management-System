package com.IntegrityTechnologies.business_manager.modules.category.mapper;

import com.IntegrityTechnologies.business_manager.modules.category.dto.CategoryDTO;
import com.IntegrityTechnologies.business_manager.modules.category.model.Category;
import org.mapstruct.*;

@Mapper(componentModel = "spring")
public interface CategoryMapper {

    @Mapping(target = "parentId", source = "parent.id")
    @Mapping(target = "subcategories", ignore = true) // handled recursively in service
    CategoryDTO toDTO(Category category);

    @Mapping(target = "parent", ignore = true) // set in service
    @Mapping(target = "subcategories", ignore = true)
    Category toEntity(CategoryDTO dto);

    @BeanMapping(nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE)
    @Mapping(target = "parent", ignore = true)
    @Mapping(target = "subcategories", ignore = true)
    void updateEntityFromDTO(CategoryDTO dto, @MappingTarget Category entity);
}