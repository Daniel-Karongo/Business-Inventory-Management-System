package com.IntegrityTechnologies.business_manager.modules.person.branch.document.model;

import com.IntegrityTechnologies.business_manager.modules.person.branch.model.Branch;
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
        name = "branch_documents",
        indexes = {
                @Index(name = "idx_branch_document_tenant", columnList = "tenant_id"),
                @Index(name = "idx_branch_document_branch", columnList = "branch_id"),
                @Index(name = "idx_branch_document_deleted", columnList = "deleted"),
                @Index(name = "idx_branch_document_type", columnList = "branch_id,document_type,deleted"),
                @Index(name = "idx_branch_document_logo", columnList = "branch_id,logo,deleted")
        },
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uq_branch_document_filename",
                        columnNames = {
                                "tenant_id",
                                "branch_id",
                                "file_name"
                        }
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class BranchDocument extends BranchAwareEntity {

    @Id
    @GeneratedValue
    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(name = "file_name", nullable = false)
    private String fileName;

    @Column(nullable = false)
    private String filePath;

    @Enumerated(EnumType.STRING)
    @Column(name = "document_type", nullable = false)
    private BranchDocumentType documentType;

    @Column(length = 1000)
    private String description;

    @Column(nullable = false)
    @Builder.Default
    private Boolean logo = false;

    private LocalDateTime uploadedAt;

    @Override
    protected void beforePersist() {
        if (logo == null) logo = false;
    }
}