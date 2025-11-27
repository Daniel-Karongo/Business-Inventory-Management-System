package com.IntegrityTechnologies.business_manager.modules.person.entity.customer.dto;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Data
public class CustomerRequest {

    @NotBlank(message = "Name is required")
    private String name;

    // optional but usually useful
    private String phone;

    @Email(message = "Email should be valid")
    private String email;

    private String address;

    private String notes;
}