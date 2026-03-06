package com.IntegrityTechnologies.business_manager.modules.platform.tenant.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.dto.CreateTenantRequest;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.dto.TenantDTO;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.mapper.TenantMapper;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.service.TenantService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/api/platform/tenants")
@RequiredArgsConstructor
public class TenantController {

    private final TenantService tenantService;

    @PostMapping
    public TenantDTO createTenant(
            @RequestBody CreateTenantRequest request
    ) {

        return TenantMapper.toDTO(
                tenantService.createTenant(
                        request.getName(),
                        request.getCode()
                )
        );

    }

    @GetMapping("/{id}")
    public TenantDTO getTenant(@PathVariable UUID id) {

        return TenantMapper.toDTO(
                tenantService.getTenant(id)
        );

    }

    @GetMapping
    public List<TenantDTO> getAll() {

        return tenantService.getAllTenants()
                .stream()
                .map(TenantMapper::toDTO)
                .collect(Collectors.toList());

    }

}