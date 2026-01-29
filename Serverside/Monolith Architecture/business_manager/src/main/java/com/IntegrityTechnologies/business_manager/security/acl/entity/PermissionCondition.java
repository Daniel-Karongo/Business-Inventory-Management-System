package com.IntegrityTechnologies.business_manager.security.acl.entity;

import jakarta.persistence.*;
import lombok.*;

import java.util.UUID;

@Entity
@Table(name = "acl_permission_conditions")
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
    private RoleEntity role;

    @ManyToOne(optional = false)
    private Permission permission;

    @Column(nullable = false)
    private String expression; // e.g. "#deleted == true"

    @Column(nullable = false)
    private boolean active = true;
}