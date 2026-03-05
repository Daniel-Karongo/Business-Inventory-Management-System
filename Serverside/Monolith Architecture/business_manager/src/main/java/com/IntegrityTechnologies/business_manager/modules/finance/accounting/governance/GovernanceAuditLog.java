package com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "governance_audit_log",
        indexes = @Index(name = "idx_gov_action_time", columnList = "performedAt"))
@Getter
@Setter
@NoArgsConstructor
public class GovernanceAuditLog {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false)
    private UUID branchId;

    @Column(nullable = false)
    private String action;

    @Column(nullable = false)
    private String performedBy;

    @Column(nullable = false)
    private LocalDateTime performedAt;

    @Column(length = 1000)
    private String details;
}