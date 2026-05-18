package com.IntegrityTechnologies.business_manager.modules.finance.payment.base.domain;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.enums.SupplierPaymentStatus;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
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
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
public class SupplierPayment
        extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @UuidGenerator
    private UUID id;

    @Column(
            name = "document_number",
            nullable = false,
            updatable = false
    )
    private String documentNumber;

    @Column(name = "supplier_id", nullable = false)
    private UUID supplierId;

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

    @Column(nullable = false)
    private String method;

    private String reference;

    @Column(
            name = "journal_entry_id",
            columnDefinition = "BINARY(16)"
    )
    private UUID journalEntryId;

    @Column(nullable = false)
    @Builder.Default
    private boolean posted = false;

    private LocalDateTime postedAt;

    private String postedBy;

    @Column(nullable = false)
    @Builder.Default
    private boolean reversed = false;

    private LocalDateTime reversedAt;

    private String reversedBy;

    private String reversalReason;

    @Column(nullable = false)
    private LocalDate paymentDate;

    @Column(nullable = false)
    private LocalDateTime paidAt;

    @Column(nullable = false)
    private String paidBy;
}