package com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.dto;

import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.Rollcall;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.RollcallMethod;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.RollcallStatus;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
public class RollcallDTO {
    private UUID rollCallId;
    private UUID userId;
    private UUID departmentId;
    private UUID branchId;
    private LocalDateTime timestamp;
    private RollcallStatus status;
    private RollcallMethod method;
    private UUID biometricRecordId;
    private String performedBy;

    public static RollcallDTO from(Rollcall rollcall) {
        RollcallDTO dto = new RollcallDTO();
        dto.setRollCallId(rollcall.getId());
        dto.setUserId(rollcall.getUser().getId());
        dto.setDepartmentId(
                rollcall.getDepartment() == null ? null : rollcall.getDepartment().getId()
        );
        dto.setBranchId(
                rollcall.getBranch() == null ? null : rollcall.getBranch().getId()
        );
        dto.setTimestamp(rollcall.getTimestamp());
        dto.setStatus(rollcall.getStatus());
        dto.setMethod(rollcall.getMethod());
        dto.setBiometricRecordId(rollcall.getBiometricRecordId());
        dto.setPerformedBy(rollcall.getPerformedBy());
        return dto;
    }
}
