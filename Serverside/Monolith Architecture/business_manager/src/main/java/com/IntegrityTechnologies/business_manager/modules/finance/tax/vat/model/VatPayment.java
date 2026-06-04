package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "vat_payment",
        indexes = {
                @Index(
                        name = "idx_vat_payment_filing",
                        columnList = "tenant_id,branch_id,filing_id"
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class VatPayment extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @ManyToOne(optional = false)
    @JoinColumn(name = "filing_id")
    private VatFiling filing;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal amount;

    @Column(nullable = false)
    private UUID fundingAccountId;

    @Column(nullable = false)
    private String recordedBy;

    @Column(nullable = false)
    private LocalDateTime recordedAt;
}