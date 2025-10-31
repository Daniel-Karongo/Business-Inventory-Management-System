package com.IntegrityTechnologies.business_manager.exception;

public class StorageFullException extends RuntimeException {
    public StorageFullException(String message) {
        super(message);
    }
}