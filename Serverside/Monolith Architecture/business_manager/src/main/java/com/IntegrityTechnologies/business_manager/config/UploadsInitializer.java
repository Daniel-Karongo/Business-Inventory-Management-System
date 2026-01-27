package com.IntegrityTechnologies.business_manager.config;

import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.nio.file.*;
import java.util.Comparator;

@Slf4j
@Component
@RequiredArgsConstructor
public class UploadsInitializer {

    private final FileStorageProperties props;
    private final FileStorageService fileStorageService;

    @PostConstruct
    public void init() {
        Path uploadsRoot = Paths.get(props.getBaseUploadDir())
                .toAbsolutePath()
                .normalize();

        try {
            Files.createDirectories(uploadsRoot);

            Files.walk(uploadsRoot)
                    .sorted(Comparator.reverseOrder())
                    .forEach(fileStorageService::hidePathIfSupported);

            log.info("✅ Uploads directory fully hidden (Windows-safe)");

        } catch (Exception e) {
            throw new RuntimeException("Failed initializing uploads", e);
        }
    }
}