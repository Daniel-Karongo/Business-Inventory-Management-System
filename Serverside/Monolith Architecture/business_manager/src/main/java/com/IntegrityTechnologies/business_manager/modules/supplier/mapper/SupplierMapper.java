package com.IntegrityTechnologies.business_manager.modules.supplier.mapper;

import com.IntegrityTechnologies.business_manager.modules.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.supplier.dto.SupplierDTO;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.SupplierImage;
import org.mapstruct.*;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Mapper(componentModel = "spring")
public interface SupplierMapper {

    @Mapping(target = "categoryIds", expression = "java(mapCategoryIds(supplier.getCategories()))")
    @Mapping(target = "imageUrls", expression = "java(mapImageUrls(supplier.getImages()))")
    SupplierDTO toDTO(Supplier supplier);

    @Mapping(target = "categories", ignore = true)
    @Mapping(target = "images", ignore = true)
    Supplier toEntity(SupplierDTO dto);

    default Set<Long> mapCategoryIds(Set<Category> categories) {
        if (categories == null) return Set.of();
        return categories.stream().map(Category::getId).collect(Collectors.toSet());
    }

    default List<String> mapImageUrls(List<SupplierImage> images) {
        if (images == null) return List.of();
        return images.stream()
                .map(SupplierImage::getFilePath)
                .collect(Collectors.toList());
    }
}