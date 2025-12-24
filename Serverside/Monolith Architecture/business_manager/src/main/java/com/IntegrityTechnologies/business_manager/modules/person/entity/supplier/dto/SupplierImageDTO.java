package com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.dto;

import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SupplierImageDTO {

    private UUID id;

    private String fileName;

    /**
     * Never rendered by UI, but useful for audits / debugging.
     * Can be null if you want to be extra safe.
     */
    private String filePath;

    private String description;

    private Boolean deleted;

    private LocalDateTime uploadedAt;
}