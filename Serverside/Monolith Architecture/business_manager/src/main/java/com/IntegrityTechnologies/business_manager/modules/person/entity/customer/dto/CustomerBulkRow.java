package com.IntegrityTechnologies.business_manager.modules.person.entity.customer.dto;

import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.model.CustomerType;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.model.Gender;
import lombok.Data;

import java.util.List;

@Data
public class CustomerBulkRow {

    private String name;

    private String phone;                // optional (single)
    private List<String> phoneNumbers;   // optional (multiple)

    private String email;                // optional (single)
    private List<String> emailAddresses; // optional (multiple)

    private CustomerType type;           // INDIVIDUAL | COMPANY
    private Gender gender;               // only for INDIVIDUAL

    private String address;
    private String notes;
}

