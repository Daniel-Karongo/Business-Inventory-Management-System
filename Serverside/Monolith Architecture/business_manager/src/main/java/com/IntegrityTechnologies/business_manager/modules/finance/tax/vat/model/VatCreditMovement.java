package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.enums.VatCreditMovementType;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "vat_credit_movements",
        indexes = {
                @Index(
                        name = "idx_vat_credit_movements",
                        columnList = "tenant_id,branch_id"
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class VatCreditMovement extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @ManyToOne
    private VatFiling filing;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private VatCreditMovementType type;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal amount;

    @Column(nullable = false)
    private LocalDateTime createdAt;
}