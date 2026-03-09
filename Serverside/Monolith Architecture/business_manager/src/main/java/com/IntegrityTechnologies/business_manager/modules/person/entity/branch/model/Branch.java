package com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model;

import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.UserBranch;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.TenantAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.hibernate.annotations.UuidGenerator;

import java.time.LocalDateTime;
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
       RELATIONS
    ======================================== */

    @OneToMany(
            mappedBy = "branch",
            cascade = CascadeType.ALL,
            orphanRemoval = true
    )
    @Builder.Default
    private Set<UserBranch> userBranches = new HashSet<>();

    @OneToMany(
            mappedBy = "branch"
    )
    @Builder.Default
    private Set<Department> departments = new HashSet<>();

    /* ========================================
       SYSTEM FIELDS
    ======================================== */

    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    @Column(nullable = false)
    @Builder.Default
    private Boolean deleted = false;

    /* ========================================
       LIFECYCLE
    ======================================== */

    @PrePersist
    protected void onCreate() {

        this.createdAt = LocalDateTime.now();

        if (this.deleted == null) {
            this.deleted = false;
        }

    }
}