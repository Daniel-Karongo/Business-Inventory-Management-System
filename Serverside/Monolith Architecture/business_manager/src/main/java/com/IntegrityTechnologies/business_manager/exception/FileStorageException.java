package com.IntegrityTechnologies.business_manager.exception;

public class FileStorageException extends RuntimeException {
    public FileStorageException(String message) { super(message); }
    public FileStorageException(String message, Throwable cause) { super(message, cause); }
}
