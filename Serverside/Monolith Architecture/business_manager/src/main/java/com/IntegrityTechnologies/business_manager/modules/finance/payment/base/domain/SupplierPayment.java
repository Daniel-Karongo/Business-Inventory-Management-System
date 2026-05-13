package com.IntegrityTechnologies.business_manager.modules.finance.payment.base.domain;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "supplier_payments",
        indexes = {
                @Index(name = "idx_supplier_payment_tenant", columnList = "tenant_id"),
                @Index(name = "idx_supplier_payment_branch", columnList = "branch_id"),
                @Index(name = "idx_supplier_payment_supplier", columnList = "supplierId")
        }
)
@Getter
@Setter
@SuperBuilder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
public class SupplierPayment extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false)
    private UUID supplierId;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal amount;

    @Column(nullable = false)
    private String method; // CASH | BANK | MPESA

    private String reference;

    @Column(nullable = false)
    private LocalDateTime paidAt;

    @Column(nullable = false)
    private String paidBy;
}