//package com.IntegrityTechnologies.business_manager.modules.acl.controller;
//
//import com.IntegrityTechnologies.business_manager.modules.acl.audit.AclAuditLog;
//import com.IntegrityTechnologies.business_manager.modules.acl.audit.AclAuditLogRepository;
//import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.PlatformAdminOnly;
//import lombok.RequiredArgsConstructor;
//import org.springframework.data.domain.PageRequest;
//import org.springframework.data.domain.Sort;
//import org.springframework.web.bind.annotation.*;
//
//import java.util.List;
//
//@RestController
//@RequestMapping("/api/admin/acl/audits")
//@RequiredArgsConstructor
//@PlatformAdminOnly
//public class AclAuditController {
//
//    private final AclAuditLogRepository repo;
//
//    @GetMapping
//    public List<AclAuditLog> recent(
//            @RequestParam(defaultValue = "50") int limit
//    ) {
//        return repo.findAll(
//                PageRequest.of(
//                        0,
//                        limit,
//                        Sort.by(Sort.Direction.DESC, "createdAt")
//                )
//        ).getContent();
//    }
//
//    @GetMapping("/{entityType}/{entityId}")
//    public List<AclAuditLog> byEntity(
//            @PathVariable String entityType,
//            @PathVariable String entityId
//    ) {
//        return repo.findByEntityTypeAndEntityIdOrderByCreatedAtDesc(
//                entityType,
//                entityId
//        );
//    }
//}