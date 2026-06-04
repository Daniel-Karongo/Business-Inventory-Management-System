package com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "corporate_tax_payment",
        indexes = {
                @Index(
                        name = "idx_corp_tax_payment_filing",
                        columnList = "tenant_id,branch_id,filing_id"
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class CorporateTaxPayment extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @ManyToOne(optional = false)
    @JoinColumn(name = "filing_id")
    private CorporateTaxFiling filing;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal amount;

    @Column(nullable = false)
    private UUID fundingAccountId;

    @Column(nullable = false)
    private String recordedBy;

    @Column(nullable = false)
    private LocalDateTime recordedAt;
}