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
        name = "corporate_tax_filing",
        uniqueConstraints = @UniqueConstraint(
                name = "uk_corp_tax_period_branch",
                columnNames = {"tenant_id","branch_id","periodId"}
        ),
        indexes = {
                @Index(name = "idx_corptax_tenant", columnList = "tenant_id"),
                @Index(name = "idx_corptax_branch", columnList = "branch_id")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class CorporateTaxFiling extends BranchAwareEntity {

    @Id
    @GeneratedValue
    private UUID id;

    private UUID periodId;

    private BigDecimal taxableProfit;
    private BigDecimal taxRate;
    private BigDecimal taxAmount;

    private boolean paid;

    private String filedBy;
    private LocalDateTime filedAt;

    private LocalDateTime paidAt;
}