package com.IntegrityTechnologies.business_manager.modules.acl.startup;

import com.IntegrityTechnologies.business_manager.modules.acl.service.PermissionCacheService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.context.annotation.DependsOn;
import org.springframework.stereotype.Component;

@Slf4j
@Component
//@DependsOn("permissionSeederService")
@RequiredArgsConstructor
public class AclCacheWarmup implements ApplicationRunner {

    private final PermissionCacheService cacheService;

    @Override
    public void run(ApplicationArguments args) {

        log.info("🔥 Warming up ACL permission cache...");

        long start = System.currentTimeMillis();

        cacheService.refresh();

        long time = System.currentTimeMillis() - start;

        log.info("✅ ACL cache ready ({} ms)", time);
    }
}