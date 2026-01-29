package com.IntegrityTechnologies.business_manager.security.acl.entity;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.time.Instant;
import java.util.UUID;

@Entity
@Table(name = "acl_permissions", indexes = {
        @Index(name = "idx_permission_code", columnList = "code", unique = true)
})
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Permission {

    @Id
    @GeneratedValue
    private UUID id;

    @Column(nullable = false, unique = true, length = 150)
    private String code; // e.g., INVENTORY_TRANSFER

    @Column(nullable = false, length = 255)
    private String description;

    @Column(nullable = false)
    private boolean active = true;

    @Column(nullable = false)
    private boolean superuserOnly = false;

    @CreationTimestamp
    private Instant createdAt;

    @UpdateTimestamp
    private Instant updatedAt;
}