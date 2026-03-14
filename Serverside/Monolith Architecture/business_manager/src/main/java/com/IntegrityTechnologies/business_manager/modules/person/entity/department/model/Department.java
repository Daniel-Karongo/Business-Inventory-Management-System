package com.IntegrityTechnologies.business_manager.modules.person.entity.department.model;

import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.UserDepartment;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalTime;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

@Entity
@Table(
        name = "departments",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_department_branch_name",
                        columnNames = {"tenant_id","branch_id","name"}
                )
        },
        indexes = {

                @Index(
                        name = "idx_department_tenant",
                        columnList = "tenant_id"
                ),

                @Index(
                        name = "idx_department_branch",
                        columnList = "branch_id"
                ),

                @Index(
                        name = "idx_department_deleted",
                        columnList = "deleted"
                ),

                @Index(
                        name = "idx_department_tenant_branch_name",
                        columnList = "tenant_id,branch_id,name"
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
public class Department extends BranchAwareEntity {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(nullable = false)
    private String name;

    private String description;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "branch_id", nullable = false, insertable = false, updatable = false)
    private Branch branch;

    @OneToMany(
            mappedBy = "department",
            cascade = CascadeType.ALL,
            orphanRemoval = true
    )
    @Builder.Default
    private Set<UserDepartment> userDepartments = new HashSet<>();

    private LocalTime rollcallStartTime;

    private Integer gracePeriodMinutes;

    @Column(nullable = false)
    private boolean deleted = false;
}