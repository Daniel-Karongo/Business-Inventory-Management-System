package com.IntegrityTechnologies.business_manager.modules.supplier.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;
import java.util.UUID;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SupplierDTO {
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private UUID id;

    private String name;
    private List<String> email;
    private List<String> phoneNumber;
    private String address;
    private String region;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private Double rating;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private List<String> imageUrls;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private Set<Long> categoryIds;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private boolean deleted;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private LocalDateTime createdAt;

    // ✅ Created By
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private UUID createdById;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private String createdByUsername;

    // ✅ Last Updated By
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private UUID lastUpdatedById;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private String lastUpdatedByUsername;
}