package com.IntegrityTechnologies.business_manager.modules.stock.inventory.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.TenantAwareEntity;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.valuation.InventoryValuationService;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import java.util.UUID;

@Entity
@Table(
        name = "tenant_inventory_settings",
        uniqueConstraints = {
                @UniqueConstraint(columnNames = {"tenant_id"})
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class TenantInventorySettings extends TenantAwareEntity {

    @Id
    @GeneratedValue
    private UUID id; // ✅ NEW PRIMARY KEY

    @Enumerated(EnumType.STRING)
    private InventoryValuationService.ValuationMethod valuationMethod;

    private Boolean locked;

    private Boolean enforcePackagingIntegrity;
    private Boolean allowPartialPackaging;
    private Boolean enableReservationsExpiry;
    private Integer reservationTtlMinutes;
}