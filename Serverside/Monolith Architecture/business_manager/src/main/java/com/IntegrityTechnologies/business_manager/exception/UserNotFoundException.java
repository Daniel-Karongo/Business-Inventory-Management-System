package com.IntegrityTechnologies.business_manager.exception;

public class UserNotFoundException extends EntityNotFoundException {
    public UserNotFoundException(String message) {
        super(message);
    }
}