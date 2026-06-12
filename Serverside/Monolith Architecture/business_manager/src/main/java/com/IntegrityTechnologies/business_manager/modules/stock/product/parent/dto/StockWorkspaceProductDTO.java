package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto;

import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class StockWorkspaceProductDTO {

    private UUID id;

    private String name;

    private String sku;

    private Long categoryId;

    private String categoryName;

    private String thumbnailFileName;

    private String primaryImageFileName;

    private Boolean deleted;

    private LocalDateTime updatedAt;

    private long variantCount;
}