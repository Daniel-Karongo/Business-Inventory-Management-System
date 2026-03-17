package com.IntegrityTechnologies.business_manager.config.bulk;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class BulkError {
    private int row;
    private String message;
}