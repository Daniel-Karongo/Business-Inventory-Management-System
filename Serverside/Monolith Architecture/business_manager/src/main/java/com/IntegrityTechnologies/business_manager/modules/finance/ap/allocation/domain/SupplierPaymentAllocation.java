package com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.domain;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.enums.ApAllocationType;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.enums.PaymentAllocationStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.domain.PurchaseInvoice;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.domain.SupplierPayment;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Index;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.hibernate.annotations.UuidGenerator;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "supplier_payment_allocations",
        indexes = {
                @Index(
                        name = "idx_ap_allocation_invoice",
                        columnList = "tenant_id,branch_id,purchase_invoice_id"
                ),
                @Index(
                        name = "idx_ap_allocation_payment",
                        columnList = "tenant_id,branch_id,supplier_payment_id"
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
public class SupplierPaymentAllocation
        extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @UuidGenerator
    private UUID id;

    @ManyToOne(optional = false)
    @JoinColumn(
            name = "purchase_invoice_id",
            nullable = false
    )
    private PurchaseInvoice purchaseInvoice;

    @ManyToOne(optional = false)
    @JoinColumn(
            name = "supplier_payment_id",
            nullable = false
    )
    private SupplierPayment supplierPayment;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private ApAllocationType allocationType;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private PaymentAllocationStatus status;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal allocatedAmount;

    @Column(nullable = false)
    private LocalDateTime allocatedAt;

    @Column(nullable = false)
    private String allocatedBy;

    @Column(nullable = false)
    @Builder.Default
    private boolean reversed = false;

    private LocalDateTime reversedAt;

    private String reversedBy;

    private String reversalReason;
}