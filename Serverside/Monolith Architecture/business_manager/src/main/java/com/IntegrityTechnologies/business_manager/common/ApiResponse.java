package com.IntegrityTechnologies.business_manager.common;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ApiResponse {

    private String status;
    private String message;
    private Object data; // can hold List<Map<String,Object>> for bulk operations

    public ApiResponse(String status, String message) {
        this.status = status;
        this.message = message;
    }
}
