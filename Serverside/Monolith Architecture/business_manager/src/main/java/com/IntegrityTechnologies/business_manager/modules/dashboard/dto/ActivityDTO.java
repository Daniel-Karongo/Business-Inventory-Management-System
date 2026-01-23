package com.IntegrityTechnologies.business_manager.modules.dashboard.dto;

import lombok.AllArgsConstructor;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@AllArgsConstructor
public class ActivityDTO {
    private String type;          // SALE | STOCK | USER | PRODUCT | SUPPLIER
    private String description;
    private String actor;
    private LocalDateTime time;
}