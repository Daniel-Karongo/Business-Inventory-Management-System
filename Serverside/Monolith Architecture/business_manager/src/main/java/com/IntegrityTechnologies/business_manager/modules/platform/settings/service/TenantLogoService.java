package com.IntegrityTechnologies.business_manager.modules.platform.settings.service;

import com.IntegrityTechnologies.business_manager.config.files.FileStorageService;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.nio.file.Path;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class TenantLogoService {

    private final FileStorageService storageService;
    private final TenantSettingsService settingsService;

    public String uploadLogo(MultipartFile file) throws IOException {

        UUID tenantId = TenantContext.getTenantId();

        Path brandingDir = storageService.brandingRoot();

        Path saved = storageService.saveFile(
                brandingDir,
                "logo.png",
                file.getInputStream()
        );

        String relative =
                storageService.toRelative(saved).toString();

        settingsService.updateLogo(relative);

        return relative;
    }

    public Path resolveLogo(String relativePath) {

        return storageService.resolveRelative(relativePath);

    }

}