package com.IntegrityTechnologies.business_manager.modules.supplier.mapper;

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

    // ===== ENTITY → DTO =====
    @Mapping(target = "imageUrls", expression = "java(mapImageUrls(supplier.getImages(), supplier.getUploadFolder()))")
    @Mapping(target = "createdById", expression = "java(mapUserId(supplier.getCreatedBy()))")
    @Mapping(target = "createdByUsername", expression = "java(mapUsername(supplier.getCreatedBy()))")
    @Mapping(target = "lastUpdatedById", expression = "java(mapUserId(supplier.getUpdatedBy()))")
    @Mapping(target = "lastUpdatedByUsername", expression = "java(mapUsername(supplier.getUpdatedBy()))")
    SupplierDTO toDTO(Supplier supplier);

    default List<SupplierDTO> toDTOList(List<Supplier> suppliers) {
        if (suppliers == null || suppliers.isEmpty()) return List.of();
        return suppliers.stream().map(this::toDTO).collect(Collectors.toList());
    }

    // ===== CREATE DTO → ENTITY =====
    @Mapping(target = "images", ignore = true)  // handled separately
    @Mapping(target = "uploadFolder", ignore = true) // set in service
    Supplier toEntity(SupplierCreateDTO dto);

    // ===== UPDATE DTO → ENTITY =====
    @Mapping(target = "images", ignore = true)
    @Mapping(target = "uploadFolder", ignore = true)
    @Mapping(target = "createdAt", ignore = true)
    @Mapping(target = "createdBy", ignore = true)
    @Mapping(target = "updatedBy", ignore = true)
    Supplier applyUpdate(@MappingTarget Supplier existing, SupplierUpdateDTO dto);

    // ===== Helper Methods =====

    default Set<String> mapImageUrls(Set<SupplierImage> images, String uploadFolder) {
        if (images == null || images.isEmpty() || uploadFolder == null) return Set.of();
        return images.stream()
                .map(img -> "/api/suppliers/images/" + uploadFolder + "/" + img.getFileName())
                .collect(Collectors.toSet());
    }

    default UUID mapUserId(User user) {
        return (user != null) ? user.getId() : null;
    }

    default String mapUsername(User user) {
        return (user != null) ? user.getUsername() : null;
    }
}