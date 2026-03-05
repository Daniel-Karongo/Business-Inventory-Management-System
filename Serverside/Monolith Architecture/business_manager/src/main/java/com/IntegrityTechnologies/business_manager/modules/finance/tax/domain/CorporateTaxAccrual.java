package com.IntegrityTechnologies.business_manager.modules.finance.tax.domain;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "corporate_tax_accruals",
        uniqueConstraints = @UniqueConstraint(columnNames = {"periodId","branchId"}))
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CorporateTaxAccrual {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    private UUID periodId;
    private UUID branchId;

    private BigDecimal taxableIncome;
    private BigDecimal taxRate;
    private BigDecimal calculatedTax;

    private LocalDateTime createdAt;
}