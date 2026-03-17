package com.IntegrityTechnologies.business_manager.common.backup.config;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;

@Getter
@Setter
@ConfigurationProperties(prefix = "database.backup")
public class DatabaseBackupProperties {

    private boolean enabled;
    private String mode;
    private String time;
    private String weekday;
    private Integer monthday;
    private String cron;
    private String dir;

    private Retention retention;
    private Encryption encryption;
    private Mysql mysql;

    @Getter @Setter
    public static class Retention {
        private int maxFiles;
        private long maxTotalSizeMb;
    }

    @Getter @Setter
    public static class Mysql {
        private String host;
        private int port;
        private String database;
        private String username;
        private String password;
        private String binPath;
    }

    @Getter @Setter
    public static class Encryption {
        private boolean enabled;
        private String secret;
    }
}