package com.IntegrityTechnologies.business_manager.modules.supplier.dto;

import lombok.*;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SupplierDTO {
    private Long id;
    private String name;
    private String email;
    private String phoneNumber;
    private String address;
    private String region;
    private Double rating;
    private List<String> imageUrls;
    private Set<Long> categoryIds;
    private boolean deleted;
    private LocalDateTime createdAt;
}