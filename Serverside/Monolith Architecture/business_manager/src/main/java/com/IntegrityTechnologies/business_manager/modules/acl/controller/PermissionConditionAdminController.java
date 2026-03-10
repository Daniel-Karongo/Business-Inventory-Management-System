//package com.IntegrityTechnologies.business_manager.modules.acl.controller;
//
//import com.IntegrityTechnologies.business_manager.modules.acl.audit.AclAuditService;
//import com.IntegrityTechnologies.business_manager.modules.acl.entity.Permission;
//import com.IntegrityTechnologies.business_manager.modules.acl.entity.PermissionCondition;
//import com.IntegrityTechnologies.business_manager.modules.acl.entity.RoleEntity;
//import com.IntegrityTechnologies.business_manager.modules.acl.repository.PermissionConditionRepository;
//import com.IntegrityTechnologies.business_manager.modules.acl.repository.PermissionRepository;
//import com.IntegrityTechnologies.business_manager.modules.acl.repository.RoleEntityRepository;
//import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
//import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.PlatformAdminOnly;
//import lombok.RequiredArgsConstructor;
//import org.springframework.web.bind.annotation.*;
//
//import java.util.List;
//import java.util.Map;
//import java.util.UUID;
//
//@RestController
//@RequestMapping("/api/admin/acl")
//@RequiredArgsConstructor
//@PlatformAdminOnly
//public class PermissionConditionAdminController {
//
//    private final PermissionConditionRepository conditionRepo;
//    private final PermissionRepository permissionRepo;
//    private final RoleEntityRepository roleRepo;
//    private final AclAuditService audit;
//
//    @GetMapping("/permissions/{permissionId}/conditions")
//    public List<PermissionCondition> list(
//            @PathVariable UUID permissionId,
//            @RequestParam String role
//    ) {
//
//        Role roleEnum = Role.valueOf(role.toUpperCase());
//
//        return conditionRepo
//                .findByPermission_IdAndRole_NameAndActiveTrue(
//                        permissionId,
//                        roleEnum
//                );
//    }
//
//    @PostMapping("/permissions/{permissionId}/conditions")
//    public PermissionCondition create(
//            @PathVariable UUID permissionId,
//            @RequestBody CreateConditionRequest req
//    ) {
//
//        Permission permission =
//                permissionRepo.findById(permissionId).orElseThrow();
//
//        Role roleEnum =
//                Role.valueOf(req.role().toUpperCase());
//
//        RoleEntity role =
//                roleRepo.findByName(roleEnum)
//                        .orElseThrow();
//
//        PermissionCondition saved =
//                conditionRepo.save(
//                        PermissionCondition.builder()
//                                .permission(permission)
//                                .role(role)
//                                .param(req.param())
//                                .operator("EQ")
//                                .value(req.value())
//                                .active(true)
//                                .build()
//                );
//
//        audit.audit(
//                "CONDITION",
//                "CREATE",
//                null,
//                saved,
//                saved.getId().toString()
//        );
//
//        return saved;
//    }
//
//    @DeleteMapping("/conditions/{conditionId}")
//    public void softDelete(
//            @PathVariable UUID conditionId,
//            @RequestBody(required = false) Map<String, String> body
//    ) {
//
//        PermissionCondition before =
//                conditionRepo.findById(conditionId).orElseThrow();
//
//        before.setActive(false);
//        conditionRepo.save(before);
//
//        audit.audit(
//                "CONDITION",
//                "DEACTIVATE",
//                before,
//                null,
//                conditionId.toString()
//        );
//    }
//
//    public record CreateConditionRequest(
//            String role,
//            String param,
//            String value
//    ) {}
//}