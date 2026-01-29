package com.IntegrityTechnologies.business_manager.security.acl.entity;

import jakarta.persistence.*;
import lombok.*;

import java.util.UUID;

@Entity
@Table(name = "acl_user_branch_scope", uniqueConstraints = {
        @UniqueConstraint(name = "uq_user_branch", columnNames = {"user_id", "branch_id"})
})
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserBranchScope {

    @Id
    @GeneratedValue
    private UUID id;

    @Column(nullable = false)
    private UUID userId;

    @Column(nullable = false)
    private UUID branchId;

    @Column(nullable = false)
    private boolean active = true;

    private String grantedBy;
}