package com.IntegrityTechnologies.business_manager.common.bulk;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class BulkError {
    private int row;
    private String message;
}