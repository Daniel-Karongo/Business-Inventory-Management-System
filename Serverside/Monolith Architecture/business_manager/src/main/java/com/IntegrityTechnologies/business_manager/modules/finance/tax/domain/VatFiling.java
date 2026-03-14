package com.IntegrityTechnologies.business_manager.modules.finance.tax.domain;

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
                columnNames = {"tenant_id","branch_id","period_id"}
        )
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
    private TaxPeriod period;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal outputVat;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal inputVat;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal vatPayable;

    private String filedBy;
    private LocalDateTime filedAt;

    private boolean paid;
}