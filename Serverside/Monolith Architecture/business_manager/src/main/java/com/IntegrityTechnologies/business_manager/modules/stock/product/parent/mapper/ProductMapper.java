package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.mapper;

import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.dto.SupplierMinimalDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductImage;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductSupplier;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.dto.ProductVariantDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import org.mapstruct.*;

import java.util.*;

@Mapper(componentModel = "spring")
public interface ProductMapper {

    /* =========================================================
       ENTITY → DTO
       ========================================================= */

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
    @Mapping(target = "updatedAt",
            expression = "java(product.getUpdatedAt() != null ? product.getUpdatedAt() : product.getCreatedAt())")
    @Mapping(target = "barcode", ignore = true) // handled at variant level
    @Mapping(target = "barcodeImagePath", ignore = true)
    ProductDTO toDTO(Product product);

    /* =========================================================
       DTO → ENTITY (CREATE)
       ========================================================= */

    @Mapping(target = "id", ignore = true)
    @Mapping(target = "category", ignore = true)
    @Mapping(target = "suppliers", ignore = true)
    @Mapping(target = "images", ignore = true)
    @Mapping(target = "variants", ignore = true)
    @Mapping(target = "deleted", constant = "false")
    Product toEntity(ProductCreateDTO dto);

    /* =========================================================
       PARTIAL UPDATE
       ========================================================= */

    @BeanMapping(nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE)
    @Mapping(target = "images", ignore = true)
    @Mapping(target = "variants", ignore = true)
    @Mapping(target = "category", ignore = true)
    @Mapping(target = "deleted", ignore = true)
    @Mapping(target = "deletedAt", ignore = true)
    Product applyUpdate(@MappingTarget Product existing, ProductUpdateDTO dto);

    /* =========================================================
       IMAGE MAPPING
       ========================================================= */

    default List<String> mapImagesToUrls(List<ProductImage> images) {
        if (images == null || images.isEmpty()) return List.of();

        List<String> urls = new ArrayList<>();

        for (ProductImage img : images) {
            if (!Boolean.TRUE.equals(img.getDeleted())) {
                urls.add(img.getFilePath());
            }
        }

        return urls;
    }

    /* =========================================================
       SUPPLIER MAPPING
       ========================================================= */

    default List<SupplierMinimalDTO> mapSuppliersMinimal(Set<ProductSupplier> productSuppliers) {

        if (productSuppliers == null || productSuppliers.isEmpty()) return List.of();

        return productSuppliers.stream()
                .map(ProductSupplier::getSupplier)
                .filter(Objects::nonNull)
                .map(s -> new SupplierMinimalDTO(s.getId(), s.getName()))
                .toList();
    }

    /* =========================================================
       VARIANT MAPPING (SAFE)
       ========================================================= */

    default List<ProductVariantDTO> mapVariants(List<ProductVariant> variants) {
        if (variants == null || variants.isEmpty()) return List.of();

        List<ProductVariantDTO> result = new ArrayList<>();

        for (ProductVariant v : variants) {

            result.add(
                    ProductVariantDTO.builder()
                            .id(v.getId())
                            .classification(v.getClassification())
                            .productId(v.getProduct() != null ? v.getProduct().getId() : null)
                            .productName(v.getProduct() != null ? v.getProduct().getName() : null)
                            .sku(v.getSku())
                            .barcode(v.getBarcode()) // ✅ KEEP
                            .barcodeImagePath(v.getBarcodeImagePath()) // ✅ KEEP
                            .averageBuyingPrice(v.getAverageBuyingPrice())
                            .minimumSellingPrice(v.getMinimumSellingPrice())
                            .build()
            );
        }

        return result;
    }
}