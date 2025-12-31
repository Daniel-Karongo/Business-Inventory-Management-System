package com.IntegrityTechnologies.business_manager.modules.finance.sales.dto;

import lombok.Builder;
import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
@Builder
public class SaleCustomerDTO {
    private UUID id;
    private String name;
    private List<String> phoneNumbers;
    private List<String> emailAddresses;
}