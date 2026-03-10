package com.IntegrityTechnologies.business_manager.modules.platform.identity.bootstrap;

import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformRole;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformUser;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.repository.PlatformUserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;

import jakarta.annotation.PostConstruct;

import java.time.LocalDateTime;
import java.util.UUID;

@Component
@RequiredArgsConstructor
@Slf4j
public class PlatformUserBootstrapService {

    private final PlatformUserRepository repository;
    private final PasswordEncoder passwordEncoder;

    @PostConstruct
    public void bootstrap() {

        if (repository.count() > 0) {
            log.info("Platform user bootstrap already initialized.");
            return;
        }

        String password = UUID.randomUUID()
                .toString()
                .replace("-", "")
                .substring(0, 16);

        PlatformUser root = PlatformUser.builder()
                .username("root")
                .password(passwordEncoder.encode(password))
                .role(PlatformRole.PLATFORM_SUPER_ADMIN)
                .createdAt(LocalDateTime.now())
                .active(true)
                .locked(false)
                .deleted(false)
                .mustChangePassword(true)
                .build();

        repository.save(root);

        log.warn("""
        ---------------------------------------------------------
        🔐 PLATFORM ROOT ACCOUNT CREATED
        
        Username: root
        Password: %s
        
        ⚠️ Change this password immediately
        ---------------------------------------------------------
        """.formatted(password));
    }
}