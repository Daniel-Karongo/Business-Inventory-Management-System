package com.IntegrityTechnologies.business_manager.modules.rollcall.model;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "rollcall_audits")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RollcallAudit {
    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    private UUID rollcallId;
    private UUID userId;
    private UUID departmentId;
    private String action; // e.g. RECORD, MODIFY, AUTO_ABSENT_MARK
    private String reason;
    private LocalDateTime timestamp;
    private String performedBy;
}
