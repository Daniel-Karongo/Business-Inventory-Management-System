package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto;

import lombok.*;

import java.util.UUID;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ProductImageDTO {

    private UUID id;

    private String fileName;

    private String filePath;

    private String thumbnailFileName;

    private Boolean deleted;

    private Boolean primaryImage;

    private Boolean deletedIndependently;
}