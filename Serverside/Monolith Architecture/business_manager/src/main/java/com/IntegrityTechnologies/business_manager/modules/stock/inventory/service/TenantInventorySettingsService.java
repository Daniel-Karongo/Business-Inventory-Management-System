package com.IntegrityTechnologies.business_manager.modules.stock.inventory.service;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.TenantInventorySettings;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.TenantInventorySettingsRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.valuation.InventoryValuationService;
import lombok.RequiredArgsConstructor;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class TenantInventorySettingsService {

    private final TenantInventorySettingsRepository repo;
    private final Environment env;

    public InventoryValuationService.ValuationMethod get(UUID tenantId) {

        return repo.findById(tenantId)
                .map(TenantInventorySettings::getValuationMethod)
                .orElseGet(() -> {
                    String def = env.getProperty("inventory.valuation.mode", "WAC");
                    return InventoryValuationService.ValuationMethod.valueOf(def);
                });
    }

    public void initialize(UUID tenantId) {

        if (repo.existsById(tenantId)) return;

        String def = env.getProperty("inventory.valuation.mode", "WAC");

        repo.save(TenantInventorySettings.builder()
                .tenantId(tenantId)
                .valuationMethod(
                        InventoryValuationService.ValuationMethod.valueOf(def)
                )
                .locked(false)
                .build());
    }

    public void lock(UUID tenantId) {

        repo.findById(tenantId).ifPresent(s -> {
            if (!s.isLocked()) {
                s.setLocked(true);
                repo.save(s);
            }
        });
    }

    public void change(UUID tenantId, String method) {

        TenantInventorySettings s = repo.findById(tenantId)
                .orElseThrow();

        if (s.isLocked()) {
            throw new IllegalStateException("Valuation method is locked");
        }

        s.setValuationMethod(
                InventoryValuationService.ValuationMethod.valueOf(method)
        );

        repo.save(s);
    }
}