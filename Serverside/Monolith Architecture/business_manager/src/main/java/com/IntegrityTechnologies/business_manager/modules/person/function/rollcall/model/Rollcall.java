package com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "rollcalls",
        uniqueConstraints = @UniqueConstraint(
                columnNames = { "user_id", "department_id", "branch_id", "rollcall_date", "method" }
        )
)
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Rollcall {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(name = "user_id", columnDefinition = "BINARY(16)", nullable = false)
    private UUID userId;
    private String username;

    @Column(name = "department_id", columnDefinition = "BINARY(16)", nullable = false)
    private UUID departmentId;
    private String departmentName;

    @Column(name = "branch_id", columnDefinition = "BINARY(16)", nullable = false)
    private UUID branchId;
    private String branchName;

    @Column(nullable = false)
    private LocalDateTime timestamp;

    @Column(name = "rollcall_date", nullable = false)
    private LocalDate rollcallDate;

    @Enumerated(EnumType.STRING)
    private RollcallStatus status;

    @Enumerated(EnumType.STRING)
    private RollcallMethod method;

    // If biometric, reference to biometric record id (if applicable)
    @Column(columnDefinition = "BINARY(16)")
    private UUID biometricRecordId;

    private String performedBy;
}