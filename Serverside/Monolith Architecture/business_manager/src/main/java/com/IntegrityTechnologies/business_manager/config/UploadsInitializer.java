package com.IntegrityTechnologies.business_manager.config;

import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.DosFileAttributeView;

@Component
@RequiredArgsConstructor
public class UploadsInitializer {

    private final FileStorageProperties props;

    @PostConstruct
    public void init() throws IOException {
        Path uploadsRoot = Paths.get(props.getUserUploadDir()).getParent();
        if (uploadsRoot != null && !Files.exists(uploadsRoot)) {
            Files.createDirectories(uploadsRoot);
        }

        // Hide it immediately
        if (System.getProperty("os.name").toLowerCase().contains("win")) {
            DosFileAttributeView attr = Files.getFileAttributeView(uploadsRoot, DosFileAttributeView.class);
            if (attr != null) attr.setHidden(true);
        }
    }
}
