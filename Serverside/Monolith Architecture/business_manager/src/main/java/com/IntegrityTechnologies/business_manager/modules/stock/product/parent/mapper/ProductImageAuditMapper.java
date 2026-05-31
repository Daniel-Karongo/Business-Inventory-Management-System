package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.mapper;

import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductImageAuditDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductImageAudit;
import org.mapstruct.Mapper;

import java.util.Collection;
import java.util.List;

@Mapper(componentModel = "spring")
public interface ProductImageAuditMapper {

    ProductImageAuditDTO toDto(
            ProductImageAudit audit
    );

    List<ProductImageAuditDTO> toDtos(
            Collection<ProductImageAudit> audits
    );
}