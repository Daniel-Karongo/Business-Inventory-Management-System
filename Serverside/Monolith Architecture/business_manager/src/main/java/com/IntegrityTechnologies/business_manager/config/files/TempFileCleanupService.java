package com.IntegrityTechnologies.business_manager.config.files;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.service.TenantExecutionService;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.nio.file.*;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.stream.Stream;

@Slf4j
@Component
@RequiredArgsConstructor
public class TempFileCleanupService {

    private final FileStorageService fileStorageService;
    private final TenantExecutionService tenantExecutionService;

    @Scheduled(fixedDelay = 10 * 60 * 1000)
    public void cleanup() {

        tenantExecutionService.forEachTenant(tenantId -> {

            try {

                Path tenantRoot = fileStorageService.tenantRoot();

                try (Stream<Path> branches = Files.list(tenantRoot)) {

                    branches.forEach(branchDir -> {

                        try {

                            Path tmpDir = branchDir
                                    .resolve(".products")
                                    .resolve("_shared")
                                    .resolve("_tmp");

                            if (!Files.exists(tmpDir)) return;

                            Instant cutoff = Instant.now().minus(30, ChronoUnit.MINUTES);

                            try (Stream<Path> files = Files.list(tmpDir)) {

                                files.forEach(path -> {
                                    try {
                                        if (Files.getLastModifiedTime(path)
                                                .toInstant()
                                                .isBefore(cutoff)) {

                                            Files.deleteIfExists(path);
                                        }
                                    } catch (IOException e) {
                                        log.warn("Failed to delete temp file {}", path, e);
                                    }
                                });

                            }

                        } catch (Exception e) {
                            log.warn("Failed branch cleanup {}", branchDir, e);
                        }
                    });

                }

            } catch (IOException e) {
                log.error("Failed tenant cleanup {}", tenantId, e);
            }
        });
    }
}