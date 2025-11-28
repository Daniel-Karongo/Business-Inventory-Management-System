package com.IntegrityTechnologies.business_manager.modules.person.function.branch.model;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.JdbcTypeCode;

import java.sql.Types;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "branch_audits")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BranchAudit {
    @Id
    @GeneratedValue
    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID branchId;

    private String branchName;

    private String action;
    private String fieldChanged;

    @Column(length = 2000)
    private String oldValue;

    @Column(length = 2000)
    private String newValue;

    private String reason;

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
