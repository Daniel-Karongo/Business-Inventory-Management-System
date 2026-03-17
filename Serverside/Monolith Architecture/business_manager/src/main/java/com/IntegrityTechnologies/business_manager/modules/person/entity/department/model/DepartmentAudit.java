package com.IntegrityTechnologies.business_manager.modules.person.entity.department.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.hibernate.annotations.JdbcTypeCode;

import java.sql.Types;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "department_audits",
        indexes = {

                @Index(
                        name = "idx_dept_audit_tenant",
                        columnList = "tenant_id"
                ),

                @Index(
                        name = "idx_dept_audit_department",
                        columnList = "department_id"
                ),

                @Index(
                        name = "idx_dept_audit_performer",
                        columnList = "performed_by_id"
                ),

                @Index(
                        name = "idx_dept_audit_timestamp",
                        columnList = "timestamp"
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class DepartmentAudit extends BranchAwareEntity {

    @Id
    @GeneratedValue
    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    /* ===========================
       TARGET
    =========================== */

    @JdbcTypeCode(Types.BINARY)
    @Column(name = "department_id", columnDefinition = "BINARY(16)")
    private UUID departmentId;

    private String departmentName;

    /* ===========================
       CHANGE
    =========================== */

    private String action;

    private String fieldChanged;

    @Column(length = 2000)
    private String oldValue;

    @Column(length = 2000)
    private String newValue;

    private String reason;

    /* ===========================
       PERFORMER
    =========================== */

    @JdbcTypeCode(Types.BINARY)
    @Column(name = "performed_by_id", columnDefinition = "BINARY(16)")
    private UUID performedById;

    private String performedByUsername;

    /* ===========================
       TIME
    =========================== */

    private LocalDateTime timestamp;

    @PrePersist
    public void onCreate() {
        timestamp = LocalDateTime.now();
    }
}