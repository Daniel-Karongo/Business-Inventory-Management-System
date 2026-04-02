package com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.dto;

import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.model.RollcallStatus;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.model.Rollcall;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.model.RollcallMethod;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
public class RollcallDTO {
    private UUID rollCallId;
    private UUID userId;
    private String userName;
    private UUID departmentId;
    private String departmentName;
    private UUID branchId;
    private String branchName;
    private LocalDateTime timestamp;
    private RollcallStatus status;
    private RollcallMethod method;
    private UUID biometricRecordId;
    private String performedBy;

    public static RollcallDTO from(Rollcall rollcall) {
        RollcallDTO dto = new RollcallDTO();
        dto.setRollCallId(rollcall.getId());
        dto.setUserId(rollcall.getUserId());
        dto.setUserName(rollcall.getUsername());
        dto.setDepartmentId(rollcall.getDepartmentId());
        dto.setDepartmentName(rollcall.getDepartmentName());
        dto.setBranchId(rollcall.getBranchId());
        dto.setBranchName(rollcall.getBranchName());
        dto.setTimestamp(rollcall.getTimestamp());
        dto.setStatus(rollcall.getStatus());
        dto.setMethod(rollcall.getMethod());
        dto.setBiometricRecordId(rollcall.getBiometricRecordId());
        dto.setPerformedBy(rollcall.getPerformedBy());
        return dto;
    }
}
