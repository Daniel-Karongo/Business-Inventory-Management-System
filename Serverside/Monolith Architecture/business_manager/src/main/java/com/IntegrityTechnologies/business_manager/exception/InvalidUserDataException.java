package com.IntegrityTechnologies.business_manager.exception;

/**
 * Exception for invalid or duplicate user input (e.g., email, username, ID number)
 */
public class InvalidUserDataException extends RuntimeException {
    public InvalidUserDataException(String message) {
        super(message);
    }
}