package com.IntegrityTechnologies.business_manager.exception;

public abstract class EntityNotFoundException extends RuntimeException {
    public EntityNotFoundException(String message) {
        super(message);
    }
}