package com.IntegrityTechnologies.business_manager.exception;

public class ExpectedConcurrencyException extends RuntimeException {
    public ExpectedConcurrencyException(String message) {
        super(message, null, false, false);
    }
}