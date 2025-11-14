package com.IntegrityTechnologies.business_manager.modules.supplier.mapper;

import com.IntegrityTechnologies.business_manager.modules.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.supplier.dto.SupplierCreateDTO;
import com.IntegrityTechnologies.business_manager.modules.supplier.dto.SupplierDTO;
import com.IntegrityTechnologies.business_manager.modules.supplier.dto.SupplierUpdateDTO;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.SupplierImage;
import com.IntegrityTechnologies.business_manager.modules.user.model.User;
import org.mapstruct.*;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

@Mapper(componentModel = "spring")
public interface SupplierMapper {

    // ✅ Map nested entities to flattened fields
    @Mapping(target = "categoryIds", expression = "java(mapCategoryIds(supplier.getCategories()))")
    @Mapping(target = "imageUrls", expression = "java(mapImageUrls(supplier.getImages(), supplier.getId()))")

    // ✅ Created By
    @Mapping(target = "createdById", expression = "java(mapUserId(supplier.getCreatedBy()))")
    @Mapping(target = "createdByUsername", expression = "java(mapUsername(supplier.getCreatedBy()))")

    // ✅ Updated By
    @Mapping(target = "lastUpdatedById", expression = "java(mapUserId(supplier.getUpdatedBy()))")
    @Mapping(target = "lastUpdatedByUsername", expression = "java(mapUsername(supplier.getUpdatedBy()))")
    SupplierDTO toDTO(Supplier supplier);

    List<SupplierDTO> toDTOList(List<Supplier> suppliers);

    // ✅ Create DTO → entity (ignore relationships)
    @Mapping(target = "images", ignore = true)
    Supplier toEntity(SupplierCreateDTO dto);

    // ✅ Partial update
    @Mapping(target = "images", ignore = true)
    @Mapping(target = "categories", ignore = true)
    @Mapping(target = "createdAt", ignore = true)
    @Mapping(target = "createdBy", ignore = true)
    @Mapping(target = "updatedBy", ignore = true)
    Supplier applyUpdate(@MappingTarget Supplier existing, SupplierUpdateDTO dto);

    // --- Helper methods ---

    default Set<Long> mapCategoryIds(Set<Category> categories) {
        if (categories == null || categories.isEmpty()) return Set.of();
        return categories.stream()
                .map(c -> c.getId())
                .collect(Collectors.toSet());
    }

    default List<String> mapImageUrls(List<SupplierImage> images, UUID supplierId) {
        if (images == null || images.isEmpty() || supplierId == null) return List.of();
        return images.stream()
                .map(img -> "/api/suppliers/" + supplierId + "/images/" + img.getFileName())
                .collect(Collectors.toList());
    }

    // ✅ User helpers
    default UUID mapUserId(User user) {
        return (user != null) ? user.getId() : null;
    }

    default String mapUsername(User user) {
        return (user != null) ? user.getUsername() : null;
    }
}