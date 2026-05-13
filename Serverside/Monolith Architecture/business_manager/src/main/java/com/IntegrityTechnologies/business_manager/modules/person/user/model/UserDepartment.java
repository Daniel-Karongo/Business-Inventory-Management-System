package com.IntegrityTechnologies.business_manager.modules.person.user.model;

import com.IntegrityTechnologies.business_manager.modules.person.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.department.model.DepartmentMembershipRole;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.TenantAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;

@Entity
@Table(
        name = "user_departments",
        uniqueConstraints = {
                @UniqueConstraint(columnNames = {"user_id", "department_id"})
        },
        indexes = {
                @Index(name = "idx_ud_user", columnList = "user_id"),
                @Index(name = "idx_ud_department", columnList = "department_id"),
                @Index(name = "idx_ud_role", columnList = "role")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class UserDepartment extends BranchAwareEntity {

    @EmbeddedId
    @Builder.Default
    private UserDepartmentId id = new UserDepartmentId();

    @ManyToOne(fetch = FetchType.LAZY)
    @MapsId("userId")
    @JoinColumn(name = "user_id")
    private User user;

    @ManyToOne(fetch = FetchType.LAZY)
    @MapsId("departmentId")
    @JoinColumn(name = "department_id")
    private Department department;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private DepartmentMembershipRole role;

    @Column(nullable = false)
    private boolean primaryDepartment = false;

    @Column(nullable = false, updatable = false)
    private LocalDateTime assignedAt;

    @Override
    public void beforePersist() {

        assignedAt = LocalDateTime.now();

        if (role == null) {
            role = DepartmentMembershipRole.MEMBER;
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof UserDepartment that)) return false;
        return id != null && id.equals(that.id);
    }

    @Override
    public int hashCode() {
        return getClass().hashCode();
    }
}