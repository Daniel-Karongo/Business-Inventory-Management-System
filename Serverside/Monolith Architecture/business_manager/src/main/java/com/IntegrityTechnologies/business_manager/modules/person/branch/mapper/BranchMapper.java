package com.IntegrityTechnologies.business_manager.modules.person.branch.mapper;

import com.IntegrityTechnologies.business_manager.modules.person.branch.dto.BranchDetailsDTO;
import com.IntegrityTechnologies.business_manager.modules.person.branch.dto.BranchListItemDTO;
import com.IntegrityTechnologies.business_manager.modules.person.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.user.dto.MinimalUserDTO;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.User;
import lombok.experimental.UtilityClass;

import java.util.Set;
import java.util.stream.Collectors;

@UtilityClass
public class BranchMapper {

    public BranchListItemDTO toListItemDTO(
            Branch branch
    ) {
        return BranchListItemDTO.builder()
                .id(branch.getId())
                .branchCode(branch.getBranchCode())
                .name(branch.getName())
                .location(branch.getLocation())
                .deleted(branch.isDeleted())
                .createdAt(branch.getCreatedAt())
                .enforceGeofence(branch.getEnforceGeofence())
                .enforceDevice(branch.getEnforceDevice())
                .build();
    }

    public BranchDetailsDTO toDetailsDTO(
            Branch branch,
            Set<User> users
    ) {

        return BranchDetailsDTO.builder()
                .id(branch.getId())
                .branchCode(branch.getBranchCode())
                .name(branch.getName())
                .location(branch.getLocation())
                .phone(branch.getPhone())
                .email(branch.getEmail())
                .latitude(branch.getLatitude())
                .longitude(branch.getLongitude())
                .radiusMeters(branch.getRadiusMeters())
                .enforceGeofence(branch.getEnforceGeofence())
                .enforceDevice(branch.getEnforceDevice())
                .rollcallStartTime(branch.getRollcallStartTime())
                .rollcallGraceMinutes(branch.getRollcallGraceMinutes())
                .logoutTime(branch.getLogoutTime())
                .deleted(branch.isDeleted())
                .createdAt(branch.getCreatedAt())
                .users(
                        users.stream()
                                .map(MinimalUserDTO::from)
                                .collect(Collectors.toSet())
                )
                .build();
    }
}