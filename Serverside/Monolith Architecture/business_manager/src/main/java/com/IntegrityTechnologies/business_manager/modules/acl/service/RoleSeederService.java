package com.IntegrityTechnologies.business_manager.modules.acl.service;

import com.IntegrityTechnologies.business_manager.modules.acl.entity.RoleEntity;
import com.IntegrityTechnologies.business_manager.modules.acl.repository.RoleEntityRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class RoleSeederService {

    private final RoleEntityRepository roleRepo;

    @PostConstruct
    public void seedRoles() {

        log.info("🔐 Syncing roles from enum...");

        for (Role role : Role.values()) {

            roleRepo.findByName(role)
                    .orElseGet(() -> {
                        RoleEntity created = roleRepo.save(
                                RoleEntity.builder()
                                        .name(role)
                                        .active(true)
                                        .build()
                        );

                        log.info("✅ Created ACL Role {}", role.name());
                        return created;
                    });
        }

        log.info("✅ Role sync complete");
    }
}