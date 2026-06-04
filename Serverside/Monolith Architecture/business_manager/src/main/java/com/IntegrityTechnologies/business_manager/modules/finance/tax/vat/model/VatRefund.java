package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.enums.VatRefundStatus;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "vat_refunds",
        indexes = {
                @Index(
                        name = "idx_vat_refund_filing",
                        columnList = "tenant_id,branch_id,filing_id"
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class VatRefund extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @ManyToOne(optional = false)
    @JoinColumn(name = "filing_id")
    private VatFiling filing;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal amount;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private VatRefundStatus status;

    @Column(nullable = false)
    private String requestedBy;

    @Column(nullable = false)
    private LocalDateTime requestedAt;

    private String processedBy;

    private LocalDateTime processedAt;
}