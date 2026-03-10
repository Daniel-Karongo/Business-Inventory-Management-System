//package com.IntegrityTechnologies.business_manager.modules.acl.controller;
//
//import com.IntegrityTechnologies.business_manager.modules.acl.service.PermissionCacheService;
//import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.PlatformAdminOnly;
//import lombok.RequiredArgsConstructor;
//import org.springframework.web.bind.annotation.*;
//
//@RestController
//@RequestMapping("/api/admin/acl/cache")
//@RequiredArgsConstructor
//@PlatformAdminOnly
//public class PermissionCacheController {
//
//    private final PermissionCacheService cache;
//
//    @PostMapping("/refresh")
//    public void refresh() {
//        cache.refresh();
//    }
//}