package com.IntegrityTechnologies.business_manager.modules.finance.tax.domain;

import jakarta.persistence.*;
import lombok.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "corporate_tax_filing",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_corp_tax_period_branch",
                        columnNames = {"periodId", "branchId"}
                )
        }
)
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CorporateTaxFiling {

    @Id
    @GeneratedValue
    private UUID id;

    private UUID periodId;

    @Column(nullable = false)
    private UUID branchId;

    private BigDecimal taxableProfit;
    private BigDecimal taxRate;
    private BigDecimal taxAmount;

    private boolean paid;

    private String filedBy;
    private LocalDateTime filedAt;

    private LocalDateTime paidAt;
}