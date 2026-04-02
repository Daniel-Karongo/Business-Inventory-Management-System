package com.IntegrityTechnologies.business_manager.modules.person.supplier.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.hibernate.annotations.UuidGenerator;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "supplier_image_audits",
        indexes = {
                @Index(name = "idx_supplier_image_audit_tenant_branch", columnList = "tenant_id, branch_id"),
                @Index(name = "idx_supplier_image_audit_supplier", columnList = "supplierId")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class SupplierImageAudit extends BranchAwareEntity {

    @Id
    @GeneratedValue
    @UuidGenerator
    private UUID id;

    private UUID supplierId;
    private String supplierName;

    private String fileName;
    private String filePath;

    private String action;
    private String reason;
    private String performedBy;

    private LocalDateTime timestamp;
}