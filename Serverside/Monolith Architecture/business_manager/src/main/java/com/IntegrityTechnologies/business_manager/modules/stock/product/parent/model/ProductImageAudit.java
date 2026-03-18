package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "product_image_audits",
        indexes = {
                @Index(name = "idx_product_image_audit_tenant_branch", columnList = "tenant_id, branch_id"),
                @Index(name = "idx_product_image_audit_product", columnList = "productId")
        }
)
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public class ProductImageAudit extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    private String fileName;
    private String filePath;

    private String action;
    private String reason;

    private LocalDateTime timestamp;

    private UUID productId;
    private String productName;

    private String performedBy;
}