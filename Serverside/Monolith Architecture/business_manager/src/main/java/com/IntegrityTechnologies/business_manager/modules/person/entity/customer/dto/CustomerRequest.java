package com.IntegrityTechnologies.business_manager.modules.person.entity.customer.dto;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
public class CustomerRequest {
    private UUID customerId;
    private String name;
    private List<String> phoneNumbers;
    private List<@Email String> emailAddresses;
    private String address;
    private String notes;
}