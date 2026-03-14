package com.IntegrityTechnologies.business_manager.modules.finance.tax.domain;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDate;
import java.util.UUID;

@Entity
@Table(
        name = "tax_periods",
        uniqueConstraints = @UniqueConstraint(
                columnNames = {"tenant_id","branch_id","startDate","endDate"}
        )
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class TaxPeriod extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    private LocalDate startDate;
    private LocalDate endDate;

    @Column(nullable = false)
    private boolean closed;

    private String closedBy;
}