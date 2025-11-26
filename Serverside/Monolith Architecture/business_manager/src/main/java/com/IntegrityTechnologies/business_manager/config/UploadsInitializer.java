package com.IntegrityTechnologies.business_manager.config;

import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.DosFileAttributeView;

@Slf4j
@Component
@RequiredArgsConstructor
public class UploadsInitializer {

    private final FileStorageProperties props;
    private final FileStorageService fileStorageService;

    @PostConstruct
    public void init() {
        createAndHide(props.getBaseUploadDir(), "Base");
        createAndHide(props.getUserUploadDir(), "User");
        createAndHide(props.getSupplierUploadDir(), "Supplier");
        createAndHide(props.getProductUploadDir(), "Product");
    }

    private void createAndHide(String dir, String label) {
        try {
            Path path = Paths.get(dir).toAbsolutePath().normalize();
            Files.createDirectories(path); // ensures existence
            fileStorageService.hidePathIfSupported(path);
            log.info("{} directory initialized: {}", label, path);
        } catch (Exception e) {
            log.warn("Failed to initialize {} directory: {}", label, e.getMessage());
        }
    }

}