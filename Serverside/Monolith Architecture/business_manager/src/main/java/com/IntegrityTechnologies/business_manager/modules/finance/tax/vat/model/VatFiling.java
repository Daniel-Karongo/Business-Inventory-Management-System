package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.enums.VatFilingStatus;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "vat_filings",
        uniqueConstraints = @UniqueConstraint(
                name = "uk_vat_period_branch",
                columnNames = {
                        "tenant_id",
                        "branch_id",
                        "period_id"
                }
        ),
        indexes = {
                @Index(
                        name = "idx_vat_tenant",
                        columnList = "tenant_id"
                ),
                @Index(
                        name = "idx_vat_branch",
                        columnList = "branch_id"
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class VatFiling extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @ManyToOne(optional = false)
    private AccountingPeriod period;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal outputVat;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal inputVat;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal vatPayable;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal openingCredit;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal creditApplied;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal closingCredit;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal vatReceivableCreated;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal paidAmount = BigDecimal.ZERO;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal outstandingAmount = BigDecimal.ZERO;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false, length = 50)
    private VatFilingStatus status;

    private String filedBy;

    private LocalDateTime filedAt;

    private LocalDateTime paidAt;
}