package com.IntegrityTechnologies.business_manager.security.acl.entity;

import jakarta.persistence.*;
import lombok.*;

@Entity
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Table(
        name = "endpoint_permissions",
        indexes = {
                @Index(name = "idx_endpoint_method_path", columnList = "httpMethod,path")
        }
)
public class EndpointPermission {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private String httpMethod;

    @Column(nullable = false)
    private String path;

    @ManyToOne(optional = false)
    @JoinColumn(name = "permission_id")
    private Permission permission;

    @Column(nullable = false)
    private boolean active = true;
}