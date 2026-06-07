package com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.model;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.enums.SupplierPaymentMethod;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.enums.SupplierPaymentStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.shared.domain.BaseFinancialDocument;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.hibernate.annotations.UuidGenerator;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "supplier_payments",
        indexes = {
                @Index(
                        name = "idx_supplier_payment_tenant",
                        columnList = "tenant_id"
                ),
                @Index(
                        name = "idx_supplier_payment_branch",
                        columnList = "branch_id"
                ),
                @Index(
                        name = "idx_supplier_payment_supplier",
                        columnList = "supplier_id"
                ),
                @Index(
                        name = "idx_supplier_payment_status",
                        columnList = "tenant_id,branch_id,status"
                ),
                @Index(
                        name = "idx_payment_unapplied",
                        columnList = "tenant_id,branch_id,supplier_id,unapplied_amount"
                ),
                @Index(
                        name = "idx_supplier_payment_funding_account",
                        columnList = "tenant_id,branch_id,funding_account_id"
                )
        },
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_supplier_payment_document_number",
                        columnNames = {
                                "tenant_id",
                                "branch_id",
                                "document_number"
                        }
                ),
                @UniqueConstraint(
                        name = "uk_supplier_payment_reference",
                        columnNames = {
                                "tenant_id",
                                "branch_id",
                                "supplier_id",
                                "reference"
                        }
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
public class SupplierPayment extends BaseFinancialDocument {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @UuidGenerator
    private UUID id;

    @Column(name = "supplier_id", nullable = false)
    private UUID supplierId;

    @Column(name = "funding_account_id", nullable = false)
    private UUID fundingAccountId;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal amount;

    @Column(nullable = false, precision = 19, scale = 2)
    @Builder.Default
    private BigDecimal allocatedAmount =
            BigDecimal.ZERO;

    @Column(nullable = false, precision = 19, scale = 2)
    @Builder.Default
    private BigDecimal unappliedAmount =
            BigDecimal.ZERO;

    @Column(nullable = false)
    @Builder.Default
    private boolean fullyAllocated = false;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private SupplierPaymentStatus status;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private SupplierPaymentMethod method;

    private String reference;

    @Column(nullable = false)
    private LocalDate paymentDate;

    @Column(nullable = false)
    private LocalDateTime paidAt;

    @Column(nullable = false)
    private String paidBy;
}