package com.IntegrityTechnologies.business_manager.modules.finance.tax.domain;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.util.UUID;

@Entity
@Table(
        name = "vat_ledger_projection",
        uniqueConstraints = @UniqueConstraint(
                columnNames = {"branchId","fiscalYear","monthNumber"}
        )
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class VatLedgerProjection {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    private UUID branchId;

    private UUID periodId;
    @Column(nullable = false)
    private int fiscalYear;

    @Column(nullable = false)
    private int monthNumber;
    @Column(precision = 19, scale = 2)
    private BigDecimal outputVat;

    @Column(precision = 19, scale = 2)
    private BigDecimal inputVat;
}