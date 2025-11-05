package com.IntegrityTechnologies.business_manager.exception;

public class ImageUploadException extends RuntimeException {
    public ImageUploadException(String msg) { super(msg); }
    public ImageUploadException(String msg, Throwable t) { super(msg, t); }
}

