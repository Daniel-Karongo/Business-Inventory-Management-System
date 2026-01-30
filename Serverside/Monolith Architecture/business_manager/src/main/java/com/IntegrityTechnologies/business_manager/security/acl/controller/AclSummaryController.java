package com.IntegrityTechnologies.business_manager.security.acl.controller;

import com.IntegrityTechnologies.business_manager.security.acl.entity.RolePermission;
import com.IntegrityTechnologies.business_manager.security.acl.repository.EndpointPermissionRepository;
import com.IntegrityTechnologies.business_manager.security.acl.repository.PermissionRepository;
import com.IntegrityTechnologies.business_manager.security.acl.repository.RolePermissionRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/api/admin/acl/summary")
@RequiredArgsConstructor
@PreAuthorize("hasRole('SUPERUSER')")
public class AclSummaryController {

    private final EndpointPermissionRepository endpointRepo;
    private final PermissionRepository permissionRepo;
    private final RolePermissionRepository rolePermissionRepo;

    @GetMapping
    public Map<String, Object> summary() {

        Map<String, Object> result = new LinkedHashMap<>();

        result.put("endpointCount", endpointRepo.count());
        result.put("permissionCount", permissionRepo.count());

        Map<String, Long> rolePermissionCounts =
                rolePermissionRepo.findAll().stream()
                        .filter(RolePermission::isActive)
                        .collect(Collectors.groupingBy(
                                rp -> rp.getRole().getName(),
                                LinkedHashMap::new,
                                Collectors.counting()
                        ));

        result.put("rolePermissionCounts", rolePermissionCounts);

        return result;
    }
}