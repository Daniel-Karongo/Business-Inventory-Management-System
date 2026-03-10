package com.IntegrityTechnologies.business_manager.modules.platform.identity.mapper;

import com.IntegrityTechnologies.business_manager.modules.platform.identity.dto.PlatformUserAuditResponse;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.dto.PlatformUserResponse;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformUser;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformUserAudit;

public class PlatformUserMapper {

    public static PlatformUserResponse toResponse(PlatformUser user) {

        return PlatformUserResponse.builder()
                .id(user.getId())
                .username(user.getUsername())
                .role(user.getRole().name())
                .active(user.isActive())
                .locked(user.isLocked())
                .emailAddresses(user.getEmailAddresses())
                .phoneNumbers(user.getPhoneNumbers())
                .idNumber(user.getIdNumber())
                .createdAt(user.getCreatedAt())
                .updatedAt(user.getUpdatedAt())
                .build();
    }

    public static PlatformUserAuditResponse toAuditResponse(
            PlatformUserAudit audit
    ) {

        return PlatformUserAuditResponse.builder()
                .id(audit.getId())
                .userId(audit.getUserId())
                .username(audit.getUsername())
                .action(audit.getAction())
                .reason(audit.getReason())
                .performedBy(audit.getPerformedBy())
                .timestamp(audit.getTimestamp())
                .build();
    }

}