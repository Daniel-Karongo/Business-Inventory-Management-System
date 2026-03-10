//package com.IntegrityTechnologies.business_manager.modules.acl.controller;
//
//import com.IntegrityTechnologies.business_manager.modules.acl.audit.AclAuditService;
//import com.IntegrityTechnologies.business_manager.modules.acl.entity.UserBranchScope;
//import com.IntegrityTechnologies.business_manager.modules.acl.repository.UserBranchScopeRepository;
//import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.PlatformAdminOnly;
//import lombok.RequiredArgsConstructor;
//import org.springframework.web.bind.annotation.*;
//
//import java.util.List;
//import java.util.UUID;
//
//@RestController
//@RequestMapping("/api/admin/acl/branch-scopes")
//@RequiredArgsConstructor
//@PlatformAdminOnly
//public class BranchScopeAdminController {
//
//    private final UserBranchScopeRepository repo;
//    private final AclAuditService audit;
//
//    @GetMapping
//    public List<UserBranchScope> list() {
//        return repo.findAll();
//    }
//
//    @PostMapping
//    public UserBranchScope assign(@RequestBody UserBranchScope scope) {
//
//        UserBranchScope saved = repo.save(scope);
//
//        audit.audit(
//                "BRANCH_SCOPE",
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
//    public void hardDelete(@PathVariable UUID id) {
//
//        UserBranchScope before = repo.findById(id).orElseThrow();
//        repo.deleteById(id);
//
//        audit.audit(
//                "BRANCH_SCOPE",
//                "DELETE",
//                before,
//                null,
//                id.toString()
//        );
//    }
//}