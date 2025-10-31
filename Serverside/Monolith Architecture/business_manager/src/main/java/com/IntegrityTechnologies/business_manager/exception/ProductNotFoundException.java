package com.IntegrityTechnologies.business_manager.exception;

public class ProductNotFoundException extends EntityNotFoundException {
    public ProductNotFoundException(String message) {
        super(message);
    }
}