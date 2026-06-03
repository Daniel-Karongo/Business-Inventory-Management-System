package com.IntegrityTechnologies.business_manager.modules.person.branch.dto;

import lombok.Data;

import java.time.LocalTime;
import java.util.List;
import java.util.UUID;

@Data
public class BranchFormDTO {

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

    private List<UUID> userIds;
}