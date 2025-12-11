package com.IntegrityTechnologies.business_manager.modules.person.entity.user.dto;

import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
public class UserBulkRestoreOrDeleteDTO {
    private List<UUID> ids;
    private String reason;
}