package com.IntegrityTechnologies.business_manager.modules.finance.tax.domain;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.util.UUID;

@Entity
@Table(
        name = "corporate_tax_ledger_projection",
        uniqueConstraints = @UniqueConstraint(
                columnNames = {"branchId","fiscalYear","monthNumber"}
        )
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CorporateTaxLedgerProjection {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    private UUID branchId;

    private int fiscalYear;

    private int monthNumber;

    @Column(precision = 19, scale = 2)
    private BigDecimal revenue;

    @Column(precision = 19, scale = 2)
    private BigDecimal expenses;

    @Column(precision = 19, scale = 2)
    private BigDecimal taxableProfit;

    @Column(precision = 19, scale = 2)
    private BigDecimal estimatedTax;
}