package com.IntegrityTechnologies.business_manager.modules.stock.category.mapper;

import com.IntegrityTechnologies.business_manager.modules.stock.category.dto.CategoryDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.dto.SupplierMinimalDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.CategorySupplier;
import org.mapstruct.*;

import java.util.*;

@Mapper(componentModel = "spring")
public interface CategoryMapper {

    @Mapping(target = "parentId", source = "parent.id")
    @Mapping(target = "parentName", source = "parent.name") // ✅ NEW
    @Mapping(target = "subcategories", ignore = true)
    @Mapping(target = "suppliers", source = "categorySuppliers", qualifiedByName = "mapSuppliersMinimal")
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
    default List<SupplierMinimalDTO> mapSuppliersMinimal(Set<CategorySupplier> categorySuppliers) {

        if (categorySuppliers == null || categorySuppliers.isEmpty()) {
            return List.of();
        }

        return categorySuppliers.stream()
                .map(cs -> cs.getSupplier())
                .map(s -> new SupplierMinimalDTO(s.getId(), s.getName()))
                .toList();
    }
}