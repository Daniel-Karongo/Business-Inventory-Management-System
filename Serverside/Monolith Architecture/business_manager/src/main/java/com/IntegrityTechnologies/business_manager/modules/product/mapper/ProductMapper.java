package com.IntegrityTechnologies.business_manager.modules.product.mapper;

import com.IntegrityTechnologies.business_manager.modules.product.dto.ProductCreateDTO;
import com.IntegrityTechnologies.business_manager.modules.product.dto.ProductDTO;
import com.IntegrityTechnologies.business_manager.modules.product.dto.ProductUpdateDTO;
import com.IntegrityTechnologies.business_manager.modules.product.model.Product;
import com.IntegrityTechnologies.business_manager.modules.product.model.ProductImage;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.user.UserMapper.UserMapper;
import org.mapstruct.*;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

@Mapper(componentModel = "spring", uses = {UserMapper.class})
public interface ProductMapper {

    // --- ENTITY → DTO ---
    @Mapping(target = "categoryId",
            expression = "java(product.getCategory() != null ? product.getCategory().getId() : null)")
    @Mapping(target = "categoryName",
            expression = "java(product.getCategory() != null ? product.getCategory().getName() : null)")
    @Mapping(target = "supplierIds",
            expression = "java(mapSuppliersToListOfUUIDIds(product.getSuppliers()))")
    @Mapping(target = "lastSupplierId",
            expression = "java(product.getLastSuppliedBy() != null ? product.getLastSuppliedBy().getId() : null)")
    @Mapping(target = "lastSupplierName",
            expression = "java(product.getLastSuppliedBy() != null ? product.getLastSuppliedBy().getName() : null)")
    @Mapping(target = "imageUrls",
            expression = "java(mapImagesToUrls(product.getImages()))")
    ProductDTO toDTO(Product product);

    // --- DTO → ENTITY ---
    @Mapping(target = "id", ignore = true)
    @Mapping(target = "category", ignore = true)
    @Mapping(target = "suppliers", ignore = true)
    @Mapping(target = "images", ignore = true) // handled manually in service
    Product toEntity(ProductCreateDTO dto);

    // --- Partial update ---
    @Mapping(target = "images", ignore = true)
    @Mapping(target = "category", ignore = true)
    @Mapping(target = "createdAt", ignore = true)
    @Mapping(target = "updatedAt", ignore = true)
    @BeanMapping(nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE)
    Product applyUpdate(@MappingTarget Product existing, ProductUpdateDTO dto);

    // --- Helper methods ---
    default List<String> mapImagesToUrls(List<ProductImage> images) {
        if (images == null || images.isEmpty()) return List.of();
        return images.stream()
                .map(ProductImage::getFilePath)
                .collect(Collectors.toList());
    }

    default List<UUID> mapSuppliersToListOfUUIDIds(Set<Supplier> suppliers) {
        if (suppliers == null || suppliers.isEmpty()) return List.of();
        return suppliers.stream()
                .map(Supplier::getId)
                .collect(Collectors.toList());
    }
}