package com.IntegrityTechnologies.business_manager.modules.finance.tax.domain;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.enums.BusinessTaxMode;
import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "tax_system_state",
        uniqueConstraints = @UniqueConstraint(columnNames = {"branchId"})
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TaxSystemState {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false, unique = true)
    private UUID branchId;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private BusinessTaxMode taxMode;

    @Column(nullable = false)
    private boolean vatEnabled;

    @Column(nullable = false)
    private BigDecimal vatRate;

    @Column(nullable = false)
    private BigDecimal corporateTaxRate;

    @Column(nullable = false)
    private boolean locked;

    private LocalDateTime lockedAt;
}