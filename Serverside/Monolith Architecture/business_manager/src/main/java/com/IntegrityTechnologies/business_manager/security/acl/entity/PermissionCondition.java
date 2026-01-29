package com.IntegrityTechnologies.business_manager.security.acl.entity;

import jakarta.persistence.*;
import lombok.*;

import java.util.UUID;

@Entity
@Table(
        name = "acl_permission_conditions",
        indexes = {
                @Index(name = "idx_perm_role_param", columnList = "permission_id, role_id, param")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class PermissionCondition {

    @Id
    @GeneratedValue
    private UUID id;

    @ManyToOne(optional = false)
    @JoinColumn(name = "permission_id")
    private Permission permission;

    @ManyToOne(optional = false)
    @JoinColumn(name = "role_id")
    private RoleEntity role;

    @Column(nullable = false)
    private String param;        // e.g. deleted, deletedUsers

    @Column(nullable = false)
    private String operator;     // EQ, NE

    @Column(nullable = false)
    private String value;        // true, false, null

    @Column(nullable = false)
    private boolean active = true;
}