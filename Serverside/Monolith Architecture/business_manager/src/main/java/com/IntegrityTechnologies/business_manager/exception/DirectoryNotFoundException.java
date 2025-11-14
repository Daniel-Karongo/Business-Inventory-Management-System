package com.IntegrityTechnologies.business_manager.exception;

public class DirectoryNotFoundException extends RuntimeException {
    public DirectoryNotFoundException(String module, String entityName, String expectedPath) {
        super(String.format("%s directory not found for '%s'. Expected path: %s", module, entityName, expectedPath));
    }
}