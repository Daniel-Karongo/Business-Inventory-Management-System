package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "product_variant_image_audits",
        indexes = {
                @Index(name = "idx_variant_image_audit_tenant_branch", columnList = "tenant_id, branch_id")
        }
)
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public class ProductVariantImageAudit extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    private UUID productVariantId;
    private String productName;
    private String classification;

    private String fileName;
    private String filePath;

    private String action;
    private String reason;

    private LocalDateTime timestamp;
    private String performedBy;
}