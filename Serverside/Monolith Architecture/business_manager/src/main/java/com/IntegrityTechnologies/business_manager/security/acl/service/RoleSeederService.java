//package com.IntegrityTechnologies.business_manager.security.acl.service;
//
//import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
//import com.IntegrityTechnologies.business_manager.security.acl.entity.RoleEntity;
//import com.IntegrityTechnologies.business_manager.security.acl.repository.RoleEntityRepository;
//import jakarta.annotation.PostConstruct;
//import lombok.RequiredArgsConstructor;
//import lombok.extern.slf4j.Slf4j;
//import org.springframework.stereotype.Service;
//
//@Slf4j
//@Service
//@RequiredArgsConstructor
//public class RoleSeederService {
//
//    private final RoleEntityRepository roleRepo;
//
//    @PostConstruct
//    public void seedRoles() {
//
//        log.info("🔐 Syncing roles from enum...");
//
//        for (Role role : Role.values()) {
//
//            roleRepo.findByNameIgnoreCase(role.name())
//                    .orElseGet(() -> {
//                        RoleEntity created = roleRepo.save(
//                                RoleEntity.builder()
//                                        .name(role.name())
//                                        .active(true)
//                                        .build()
//                        );
//
//                        log.info("✅ Created ACL Role {}", role.name());
//                        return created;
//                    });
//        }
//
//        log.info("✅ Role sync complete");
//    }
//}