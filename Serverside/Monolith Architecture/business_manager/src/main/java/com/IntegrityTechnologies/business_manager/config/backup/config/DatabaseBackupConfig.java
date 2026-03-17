package com.IntegrityTechnologies.business_manager.common.backup.config;

import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;

@Configuration
@EnableScheduling
@EnableConfigurationProperties(DatabaseBackupProperties.class)
public class DatabaseBackupConfig {
}