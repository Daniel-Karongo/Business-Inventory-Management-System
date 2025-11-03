package com.IntegrityTechnologies.business_manager.modules.product.mapper;

import com.IntegrityTechnologies.business_manager.modules.product.dto.ProductDTO;
import com.IntegrityTechnologies.business_manager.modules.product.model.Product;
import com.IntegrityTechnologies.business_manager.modules.product.model.ProductImage;
import com.IntegrityTechnologies.business_manager.modules.user.UserMapper.UserMapper;
import com.IntegrityTechnologies.business_manager.modules.user.model.User;
import org.mapstruct.*;

import java.util.List;
import java.util.stream.Collectors;

@Mapper(componentModel = "spring", uses = {UserMapper.class})
public interface ProductMapper {

    // --- ENTITY → DTO ---
    @Mapping(target = "categoryId",
            expression = "java(product.getCategory() != null ? product.getCategory().getId() : null)")
    @Mapping(target = "supplierId",
            expression = "java(product.getSupplier() != null ? product.getSupplier().getId() : null)")
    @Mapping(target = "imageUrls",
            expression = "java(mapImagesToUrls(product.getImages()))")
    @Mapping(target = "lastSuppliedBy",
            expression = "java(mapUserToString(product.getLastSuppliedBy()))")
    @Mapping(target = "lastModifiedBy", ignore = true) // handled elsewhere if needed
    ProductDTO toDTO(Product product);

    // --- DTO → ENTITY ---
    @Mapping(target = "id", ignore = true)
    @Mapping(target = "category", ignore = true)
    @Mapping(target = "supplier", ignore = true)
    @Mapping(target = "lastSuppliedBy", ignore = true)
    @Mapping(target = "images", ignore = true) // handled manually in service
    Product toEntity(ProductDTO dto);

    // --- Partial update ---
    @BeanMapping(nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE)
    void updateEntityFromDTO(ProductDTO dto, @MappingTarget Product product);

    // --- Helper methods ---
    default String mapUserToString(User user) {
        if (user == null) return null;
        String username = user.getUsername() != null ? user.getUsername() : "unknown";
        return user.getId() + " | " + username;
    }

    default List<String> mapImagesToUrls(List<ProductImage> images) {
        if (images == null) return null;
        return images.stream()
                .map(ProductImage::getFilePath)
                .collect(Collectors.toList());
    }
}