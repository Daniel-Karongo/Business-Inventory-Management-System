//package com.IntegrityTechnologies.business_manager.modules.acl.controller;
//
//import com.IntegrityTechnologies.business_manager.modules.acl.audit.AclAuditService;
//import com.IntegrityTechnologies.business_manager.modules.acl.entity.RolePermission;
//import com.IntegrityTechnologies.business_manager.modules.acl.repository.RolePermissionRepository;
//import com.IntegrityTechnologies.business_manager.modules.acl.service.PermissionCacheService;
//import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.PlatformAdminOnly;
//import lombok.RequiredArgsConstructor;
//import org.springframework.web.bind.annotation.*;
//
//import java.util.List;
//import java.util.UUID;
//
//@RestController
//@RequestMapping("/api/admin/acl/role-permissions")
//@RequiredArgsConstructor
//@PlatformAdminOnly
//public class RolePermissionAdminController {
//
//    private final RolePermissionRepository repo;
//    private final AclAuditService audit;
//    private final PermissionCacheService cache;
//
//    @GetMapping
//    public List<RolePermission> list() {
//        return repo.findAll();
//    }
//
//    @PostMapping
//    public RolePermission assign(@RequestBody RolePermission rp) {
//
//        RolePermission saved = repo.save(rp);
//
//        audit.audit(
//                "ROLE_PERMISSION",
//                "CREATE",
//                null,
//                saved,
//                saved.getId().toString()
//        );
//
//        return saved;
//    }
//
//    @DeleteMapping("/{id}")
//    public void softDelete(@PathVariable UUID id) {
//
//        RolePermission rp = repo.findById(id).orElseThrow();
//
//        repo.softDeleteById(id);
//        cache.refresh();
//
//        audit.audit(
//                "ROLE_PERMISSION",
//                "DEACTIVATE",
//                rp,
//                null,
//                id.toString()
//        );
//    }
//}