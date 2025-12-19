package com.IntegrityTechnologies.business_manager.modules.person.entity.customer.dto;

import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
public class CustomerSmsRequest {
    private List<UUID> customerIds;
    private String message;
}