package com.IntegrityTechnologies.business_manager.security.acl.entity;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;
import java.util.UUID;

@Entity
@Table(name = "acl_role_permissions", uniqueConstraints = {
        @UniqueConstraint(name = "uq_role_permission", columnNames = {"role_id", "permission_id"})
})
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RolePermission {

    @Id
    @GeneratedValue
    private UUID id;

    @ManyToOne(optional = false)
    @JoinColumn(name = "role_id")
    private RoleEntity role;

    @ManyToOne(optional = false)
    @JoinColumn(name = "permission_id")
    private Permission permission;

    @Column(nullable = false)
    private boolean allowed = true;

    @Column(nullable = false)
    private boolean active = true;
    private Instant grantedAt;

    private String grantedBy;
}