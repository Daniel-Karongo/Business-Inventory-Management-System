package com.IntegrityTechnologies.business_manager.common.backup.security;

import com.IntegrityTechnologies.business_manager.config.FileStorageService;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.SecureRandom;
import java.util.Arrays;
import java.util.Base64;

@Component
@Slf4j
@RequiredArgsConstructor
public class BackupKeyProvider {

    private static final int KEY_BYTES = 16;

    private final FileStorageService fileStorageService;

    private String key;

    @PostConstruct
    public void init() {

        Path securityDir = fileStorageService.internalRoot("security");
        Path keyFile = securityDir.resolve("backup.key");

        try {
            if (Files.exists(keyFile)) {
                key = Files.readString(keyFile).trim();
            } else {
                byte[] raw = new byte[KEY_BYTES];
                new SecureRandom().nextBytes(raw);
                key = Base64.getEncoder().encodeToString(raw);
                Files.writeString(keyFile, key);
                fileStorageService.secure(keyFile);
            }

            log.info("🔐 Backup key loaded (fingerprint={})",
                    Arrays.hashCode(Base64.getDecoder().decode(key)));

        } catch (IOException e) {
            throw new IllegalStateException("Failed to initialize backup encryption key", e);
        }
    }

    public String getKey() {
        return key;
    }
}