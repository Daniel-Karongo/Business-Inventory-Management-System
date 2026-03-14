package com.IntegrityTechnologies.business_manager.modules.finance.tax.domain;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.enums.BusinessTaxMode;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "tax_system_state",
        uniqueConstraints = @UniqueConstraint(
                columnNames = {"tenant_id","branch_id"}
        )
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class TaxSystemState extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private BusinessTaxMode taxMode;

    @Column(nullable = false)
    private boolean vatEnabled;

    @Column(nullable = false)
    private boolean pricesVatInclusive;

    @Column(nullable = false, precision = 19, scale = 6)
    private BigDecimal vatRate;

    @Column(nullable = false, precision = 19, scale = 6)
    private BigDecimal corporateTaxRate;

    @Column(nullable = false)
    private boolean locked;

    private LocalDateTime lockedAt;
}