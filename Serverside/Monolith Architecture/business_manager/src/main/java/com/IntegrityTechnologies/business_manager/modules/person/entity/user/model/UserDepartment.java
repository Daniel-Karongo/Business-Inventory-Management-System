package com.IntegrityTechnologies.business_manager.modules.person.entity.user.model;

import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.DepartmentMembershipRole;
import jakarta.persistence.*;
import lombok.*;

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
@Builder
public class UserDepartment {

    @EmbeddedId
    private UserDepartmentId id;

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

    private LocalDateTime assignedAt;

    @PrePersist
    public void onCreate() {
        assignedAt = LocalDateTime.now();
        if (role == null) {
            role = DepartmentMembershipRole.MEMBER;
        }
    }
}