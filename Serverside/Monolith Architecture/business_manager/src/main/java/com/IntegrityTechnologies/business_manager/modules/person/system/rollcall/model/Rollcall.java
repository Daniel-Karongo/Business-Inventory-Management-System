package com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "rollcalls",
        uniqueConstraints = @UniqueConstraint(
                columnNames = { "tenant_id", "user_id", "department_id", "branch_id", "rollcall_date" }
        )
)
@Getter
@Setter
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public class Rollcall extends BranchAwareEntity {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(name = "user_id", nullable = false, columnDefinition = "BINARY(16)")
    private UUID userId;

    private String username;

    @Column(name = "department_id", columnDefinition = "BINARY(16)")
    private UUID departmentId;

    private String departmentName;

    private String branchName;

    @Column(nullable = false)
    private LocalDateTime timestamp;

    @Column(name = "rollcall_date", nullable = false)
    private LocalDate rollcallDate;

    @Enumerated(EnumType.STRING)
    private RollcallStatus status;

    @Enumerated(EnumType.STRING)
    private RollcallMethod method;

    private String performedBy;
}