package com.IntegrityTechnologies.business_manager.modules.finance.payment.base.model;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "payments",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_payment_provider_reference",
                        columnNames = {
                                "tenant_id",
                                "branch_id",
                                "provider_reference"
                        }
                ),
                @UniqueConstraint(
                        name = "uk_payment_transaction_code",
                        columnNames = {
                                "tenant_id",
                                "branch_id",
                                "transaction_code"
                        }
                )
        },
        indexes = {
                @Index(
                        name = "idx_payment_tenant_branch",
                        columnList = "tenant_id, branch_id"
                ),
                @Index(
                        name = "idx_payment_timestamp",
                        columnList = "timestamp"
                ),
                @Index(
                        name = "idx_payment_sale",
                        columnList = "sale_id"
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
public class Payment extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "sale_id", nullable = false)
    private Sale sale;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal amount;

    @Column(nullable = false)
    private String method;

    @Column(name = "provider_reference")
    private String providerReference;

    @Column(nullable = false)
    private LocalDateTime timestamp;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private PaymentStatus status;

    private String note;

    @Column(name = "transaction_code", nullable = false)
    private String transactionCode;
}