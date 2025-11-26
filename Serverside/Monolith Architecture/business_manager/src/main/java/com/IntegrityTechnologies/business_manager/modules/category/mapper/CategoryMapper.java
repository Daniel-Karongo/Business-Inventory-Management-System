package com.IntegrityTechnologies.business_manager.modules.category.mapper;

import com.IntegrityTechnologies.business_manager.modules.category.dto.CategoryDTO;
import com.IntegrityTechnologies.business_manager.modules.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.supplier.dto.SupplierDTO;
import com.IntegrityTechnologies.business_manager.modules.supplier.dto.SupplierMinimalDTO;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.Supplier;
import org.mapstruct.*;

import java.util.*;
import java.util.stream.Collectors;

@Mapper(componentModel = "spring")
public interface CategoryMapper {

    @Mapping(target = "parentId", source = "parent.id")
    @Mapping(target = "subcategories", ignore = true) // handled recursively in service
    @Mapping(target = "suppliers", source = "suppliers", qualifiedByName = "mapSuppliersMinimal")
    CategoryDTO toDTO(Category category);

    @Mapping(target = "parent", ignore = true) // set in service
    @Mapping(target = "subcategories", ignore = true)
    Category toEntity(CategoryDTO dto);

    @BeanMapping(nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE)
    @Mapping(target = "parent", ignore = true)
    @Mapping(target = "subcategories", ignore = true)
    void updateEntityFromDTO(CategoryDTO dto, @MappingTarget Category entity);

    // Convert Set<Supplier> → List<SupplierDTO> with only id and name
    @Named("mapSuppliersMinimal")
    default List<SupplierMinimalDTO> mapSuppliersMinimal(Set<Supplier> suppliers) {
        if (suppliers == null || suppliers.isEmpty()) return List.of();
        return suppliers.stream()
                .map(s -> new SupplierMinimalDTO(s.getId(), s.getName()))
                .toList();
    }
}