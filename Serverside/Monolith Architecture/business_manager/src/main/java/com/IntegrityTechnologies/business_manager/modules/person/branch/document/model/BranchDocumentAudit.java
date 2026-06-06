package com.IntegrityTechnologies.business_manager.modules.person.branch.document.model;

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
        name = "branch_document_audits",
        indexes = {
                @Index(name = "idx_branch_document_audit_branch",columnList = "tenant_id,branch_id"),
                @Index(name = "idx_branch_document_audit_performer",columnList = "tenant_id,performed_by_id")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class BranchDocumentAudit extends BranchAwareEntity {

    @Id
    @GeneratedValue
    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    private String branchName;

    private String fileName;

    private String filePath;

    private String action;

    private String reason;

    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID performedById;

    private String performedByUsername;

    private LocalDateTime timestamp;

    @Override
    protected void beforePersist() {
        if (timestamp == null) timestamp = LocalDateTime.now();
    }
}