package com.IntegrityTechnologies.business_manager.modules.person.function.department.model;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.JdbcTypeCode;

import java.sql.Types;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "department_audits")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class DepartmentAudit {

    @Id
    @GeneratedValue
    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID departmentId;

    private String departmentName;

    private String action;       // CREATE, UPDATE, DELETE, DEMOTE, PROMOTE, etc.
    private String fieldChanged; // Optional, e.g., "name", "gracePeriodMinutes"

    @Column(length = 2000)
    private String oldValue;

    @Column(length = 2000)
    private String newValue;

    private String reason;       // Optional reason for the change

    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID performedById;

    private String performedByUsername;

    private LocalDateTime timestamp;

    @PrePersist
    public void onCreate() {
        timestamp = LocalDateTime.now();
    }
}
