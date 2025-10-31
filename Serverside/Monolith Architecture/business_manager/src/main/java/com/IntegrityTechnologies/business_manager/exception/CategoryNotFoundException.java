package com.IntegrityTechnologies.business_manager.exception;

public class CategoryNotFoundException extends EntityNotFoundException {
    public CategoryNotFoundException(String message) {
        super(message);
    }
}