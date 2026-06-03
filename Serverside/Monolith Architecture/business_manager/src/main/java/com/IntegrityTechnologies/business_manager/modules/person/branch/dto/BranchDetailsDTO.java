package com.IntegrityTechnologies.business_manager.modules.person.branch.dto;

import com.IntegrityTechnologies.business_manager.modules.person.user.dto.MinimalUserDTO;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Set;
import java.util.UUID;

@Data
@Builder
public class BranchDetailsDTO {

    private UUID id;

    private String branchCode;

    private String name;

    private String location;

    private String phone;

    private String email;

    private Double latitude;

    private Double longitude;

    private Integer radiusMeters;

    private Boolean enforceGeofence;

    private Boolean enforceDevice;

    private LocalTime rollcallStartTime;

    private Integer rollcallGraceMinutes;

    private LocalTime logoutTime;

    private Boolean deleted;

    private LocalDateTime createdAt;

    private Set<MinimalUserDTO> users;
}