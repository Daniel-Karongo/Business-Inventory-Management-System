package com.IntegrityTechnologies.business_manager.exception;

public class ConcurrentUpdateException
        extends RuntimeException{

    public ConcurrentUpdateException(
            String message,
            Throwable cause
    ){
        super(message,cause);
    }

    public ConcurrentUpdateException(
            String message
    ){
        super(message);
    }
}