package com.IntegrityTechnologies.business_manager.modules.finance.tax.domain;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.util.UUID;

@Entity
@Table(
        name = "vat_ledger_projection",
        uniqueConstraints = @UniqueConstraint(
                columnNames = {"tenant_id","branch_id","fiscalYear","monthNumber"}
        )
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class VatLedgerProjection extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    private UUID periodId;

    private int fiscalYear;
    private int monthNumber;

    @Column(precision = 19, scale = 2)
    private BigDecimal outputVat;

    @Column(precision = 19, scale = 2)
    private BigDecimal inputVat;
}