package com.IntegrityTechnologies.business_manager.config;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * File storage configuration.
 * All paths are resolved RELATIVE to the JVM working directory (user.dir).
 */
@Getter
@Setter
@Component
@ConfigurationProperties(prefix = "file")
public class FileStorageProperties {

    /* ============================================================
       NEW LOGICAL CONFIG (RELATIVE)
       ============================================================ */

    /** Root storage directory (e.g. data) */
    private String rootDir;

    /** Uploads directory under root (e.g. uploads) */
    private String uploadsDir;
}