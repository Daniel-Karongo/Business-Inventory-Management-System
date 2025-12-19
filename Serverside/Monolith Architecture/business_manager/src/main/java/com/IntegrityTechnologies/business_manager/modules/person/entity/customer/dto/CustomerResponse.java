package com.IntegrityTechnologies.business_manager.modules.person.entity.customer.dto;

import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.model.CustomerType;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.model.Gender;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
@Builder
public class CustomerResponse {
    private UUID id;
    private String name;
    private List<String> phoneNumbers;
    private List<String> email;
    private CustomerType type;
    private Gender gender; // only for INDIVIDUAL
    private Boolean deleted;
    private String address;
    private String notes;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}