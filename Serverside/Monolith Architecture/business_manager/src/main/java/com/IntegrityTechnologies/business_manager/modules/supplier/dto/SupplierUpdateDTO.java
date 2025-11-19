package com.IntegrityTechnologies.business_manager.modules.supplier.dto;

import lombok.*;

import java.util.Set;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SupplierUpdateDTO {
    private String name;
    private Set<String> email;
    private Set<String> phoneNumber;
    private String address;
    private String region;
    private Double rating;
    private Set<Long> categoryIds;
}