package com.IntegrityTechnologies.business_manager.modules.platform.identity.bootstrap;

import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformRole;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformUser;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.repository.PlatformUserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.ApplicationRunner;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.time.LocalDateTime;
import java.util.UUID;

@Configuration
@RequiredArgsConstructor
@Slf4j
public class PlatformUserBootstrapService {

    private final PlatformUserRepository repository;
    private final PasswordEncoder passwordEncoder;

    @Bean
    public ApplicationRunner platformRootBootstrapRunner() {

        return args -> {

            boolean rootExists =
                    repository.findByUsernameAndDeletedFalse("root")
                            .isPresent();

            if (rootExists) {
                return;
            }

            String password =
                    UUID.randomUUID()
                            .toString()
                            .replace("-", "")
                            .substring(0, 16);

            PlatformUser root =
                    PlatformUser.builder()
                            .username("root")
                            .password(
                                    passwordEncoder.encode(password)
                            )
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
            PLATFORM ROOT ACCOUNT CREATED

            Username: root
            Password: {}

            Change this password immediately
            ---------------------------------------------------------
            """, password);
        };
    }
}