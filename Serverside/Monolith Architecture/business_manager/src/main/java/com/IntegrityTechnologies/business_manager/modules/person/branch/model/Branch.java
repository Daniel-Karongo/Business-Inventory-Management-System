package com.IntegrityTechnologies.business_manager.modules.person.branch.model;

import com.IntegrityTechnologies.business_manager.modules.person.branch.deletion.BranchDeletionMode;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.UserBranch;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.TenantAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.hibernate.annotations.UuidGenerator;

import java.time.LocalTime;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

@Entity
@Table(
        name = "branches",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_branch_code_per_tenant",
                        columnNames = {"tenant_id", "branch_code"}
                )
        },
        indexes = {
                @Index(name = "idx_branch_tenant", columnList = "tenant_id"),
                @Index(name = "idx_branch_tenant_deleted", columnList = "tenant_id, deleted"),
                @Index(name = "idx_branch_tenant_code", columnList = "tenant_id, branch_code")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
@ToString
public class Branch extends TenantAwareEntity {

    @Id
    @GeneratedValue
    @UuidGenerator
    private UUID id;

    @Column(name = "branch_code", nullable = false, updatable = false)
    private String branchCode;

    @Column(nullable = false)
    private String name;

    private String location;
    private String phone;
    private String email;

    /* ========================================
       SECURITY (GEOFENCE + DEVICE POLICY)
    ======================================== */

    private Double latitude;
    private Double longitude;
    private Integer radiusMeters;

    @Column(nullable = false)
    @Builder.Default
    private Boolean enforceGeofence = true;

    @Column(nullable = false)
    @Builder.Default
    private Boolean enforceDevice = true;

    @Column(nullable = false)
    @Builder.Default
    private Integer maxActiveSessionsPerUser = 4;

    /* ========================================
       RELATIONS
    ======================================== */

    @OneToMany(mappedBy = "branch", cascade = CascadeType.ALL, orphanRemoval = true)
    @Builder.Default
    private Set<UserBranch> userBranches = new HashSet<>();

    @Column(name = "rollcall_start_time")
    private LocalTime rollcallStartTime;

    @Column(name = "rollcall_grace_minutes")
    private Integer rollcallGraceMinutes;

    @Column(name = "logout_time")
    private LocalTime logoutTime;

    @Column(name = "deletion_mode")
    @Enumerated(EnumType.STRING)
    private BranchDeletionMode deletionMode;
}