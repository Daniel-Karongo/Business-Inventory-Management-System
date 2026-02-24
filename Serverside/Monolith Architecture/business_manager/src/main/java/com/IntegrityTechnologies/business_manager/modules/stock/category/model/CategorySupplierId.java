package com.IntegrityTechnologies.business_manager.modules.stock.category.model;

import jakarta.persistence.Embeddable;
import lombok.*;

import java.io.Serializable;
import java.util.UUID;

@Embeddable
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class CategorySupplierId implements Serializable {

    private Long categoryId;
    private UUID supplierId;
}