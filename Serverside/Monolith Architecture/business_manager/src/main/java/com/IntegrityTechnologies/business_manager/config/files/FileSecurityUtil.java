package com.IntegrityTechnologies.business_manager.config.files;

public class FileSecurityUtil {

    public static String sanitizeFilename(String filename) {
        if (filename == null || filename.contains("..") || filename.contains("/") || filename.contains("\\")) {
            throw new IllegalArgumentException("Invalid filename");
        }
        return filename;
    }
}