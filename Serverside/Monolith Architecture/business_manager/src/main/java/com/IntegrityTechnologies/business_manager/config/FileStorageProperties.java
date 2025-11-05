package com.IntegrityTechnologies.business_manager.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Component
@ConfigurationProperties(prefix = "file")
public class FileStorageProperties {
    /** Base directory for all images */
    private String baseUploadDir;

    /** Base directory for product images */
    private String productUploadDir;

    /** Base directory for user ID images */
    private String userUploadDir;

    /** Base directory for Suppliers' images */
    private String supplierUploadDir;
}