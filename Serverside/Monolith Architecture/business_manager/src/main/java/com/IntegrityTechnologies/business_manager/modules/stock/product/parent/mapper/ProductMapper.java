package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.mapper;

import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductCreateDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductUpdateDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductImage;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.dto.SupplierMinimalDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.UserMapper.UserMapper;
import org.mapstruct.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.dto.ProductVariantDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Mapper(componentModel = "spring", uses = {UserMapper.class})
public interface ProductMapper {

    // --- ENTITY → DTO ---
    @Mapping(target = "categoryId",
            expression = "java(product.getCategory() != null ? product.getCategory().getId() : null)")
    @Mapping(target = "categoryName",
            expression = "java(product.getCategory() != null ? product.getCategory().getName() : null)")
    @Mapping(target = "suppliers",
            expression = "java(mapSuppliersMinimal(product.getSuppliers()))")
    @Mapping(target = "imageUrls",
            expression = "java(mapImagesToUrls(product.getImages()))")
    @Mapping(target = "variants",
            expression = "java(mapVariants(product.getVariants()))")
    @Mapping(target = "minimumPercentageProfit", source = "minimumPercentageProfit")
    ProductDTO toDTO(Product product);

    // --- DTO → ENTITY ---
    @Mapping(target = "id", ignore = true)
    @Mapping(target = "category", ignore = true)
    @Mapping(target = "suppliers", ignore = true)
    @Mapping(target = "images", ignore = true) // handled manually in service
    @Mapping(target = "variants", ignore = true)
    @Mapping(target = "minimumPercentageProfit", source = "minimumPercentageProfit")
    Product toEntity(ProductCreateDTO dto);

    // --- Partial update ---
    @Mapping(target = "images", ignore = true)
    @Mapping(target = "category", ignore = true)
    @Mapping(target = "createdAt", ignore = true)
    @Mapping(target = "updatedAt", ignore = true)
    @Mapping(target = "minimumPercentageProfit", source = "minimumPercentageProfit")
    @BeanMapping(nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE)
    Product applyUpdate(@MappingTarget Product existing, ProductUpdateDTO dto);

    // --- Helper methods ---
    default List<String> mapImagesToUrls(List<ProductImage> images) {
        if (images == null || images.isEmpty()) return List.of();
        return images.stream()
                .map(ProductImage::getFilePath)
                .collect(Collectors.toList());
    }

    default List<SupplierMinimalDTO> mapSuppliersMinimal(Set<Supplier> suppliers) {
        if (suppliers == null || suppliers.isEmpty()) return List.of();
        return suppliers.stream()
                .map(s -> new SupplierMinimalDTO(s.getId(), s.getName()))
                .toList();
    }

    default List<ProductVariantDTO> mapVariants(List<ProductVariant> variants) {
        if (variants == null || variants.isEmpty()) return List.of();
        return variants.stream()
                .map(v -> ProductVariantDTO.builder()
                        .id(v.getId())
                        .classification(v.getClassification())
                        .productId(v.getProduct().getId())
                        .productName(v.getProduct().getName())
                        .sku(v.getSku())
                        .averageBuyingPrice(v.getAverageBuyingPrice())
                        .minimumSellingPrice(v.getMinimumSellingPrice())
                        .build()
                )
                .toList();
    }

}