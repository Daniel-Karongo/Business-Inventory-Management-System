package com.IntegrityTechnologies.business_manager.modules.person.user.model;

import jakarta.persistence.Embeddable;
import lombok.*;

import java.io.Serializable;
import java.util.UUID;

@Embeddable
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class UserBranchId implements Serializable {

    private UUID userId;
    private UUID branchId;
}