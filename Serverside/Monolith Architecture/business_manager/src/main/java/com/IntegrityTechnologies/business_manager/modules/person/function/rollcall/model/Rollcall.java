package com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "rollcalls")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Rollcall {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", nullable = false)
    private User user;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "department_id")
    private Department department;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "branch_id")
    private Branch branch;

    private LocalDateTime timestamp; // when recorded

    // PRESENT, LATE, ABSENT (ABSENT entries may be synthesized by scheduled job)
    @Enumerated(EnumType.STRING)
    private RollcallStatus status;

    // Method: BIO_METRIC or LOGIN
    @Enumerated(EnumType.STRING)
    private RollcallMethod method;

    // If biometric, reference to biometric record id (if applicable)
    @Column(columnDefinition = "BINARY(16)")
    private UUID biometricRecordId;

    private String performedBy; // username who recorded (system / user)
}