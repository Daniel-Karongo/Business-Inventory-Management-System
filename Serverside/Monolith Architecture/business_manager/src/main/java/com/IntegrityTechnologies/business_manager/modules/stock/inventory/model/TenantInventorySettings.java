package com.IntegrityTechnologies.business_manager.modules.stock.inventory.model;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.valuation.InventoryValuationService;
import jakarta.persistence.*;
import lombok.*;

import java.util.UUID;

@Entity
@Table(name = "tenant_inventory_settings")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TenantInventorySettings {

    @Id
    private UUID tenantId;

    @Enumerated(EnumType.STRING)
    private InventoryValuationService.ValuationMethod valuationMethod;

    private boolean locked;
}
