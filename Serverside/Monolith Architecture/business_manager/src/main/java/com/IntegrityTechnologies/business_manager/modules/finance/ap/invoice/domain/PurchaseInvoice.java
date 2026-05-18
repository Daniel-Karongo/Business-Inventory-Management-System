package com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.domain;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.enums.PurchaseInvoicePostingStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.enums.PurchaseInvoiceStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.shared.domain.BaseFinancialDocument;
import com.IntegrityTechnologies.business_manager.modules.person.supplier.model.Supplier;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Index;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.hibernate.annotations.UuidGenerator;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.UUID;

@Entity
@Table(
        name = "purchase_invoices",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_purchase_invoice_document_number",
                        columnNames = {
                                "tenant_id",
                                "branch_id",
                                "documentNumber"
                        }
                ),
                @UniqueConstraint(
                        name = "uk_purchase_invoice_supplier_invoice_number",
                        columnNames = {
                                "tenant_id",
                                "branch_id",
                                "supplier_invoice_number"
                        }
                )
        },
        indexes = {
                @Index(
                        name = "idx_purchase_invoice_supplier",
                        columnList = "tenant_id,branch_id,supplier_id"
                ),
                @Index(
                        name = "idx_purchase_invoice_status",
                        columnList = "tenant_id,branch_id,status"
                ),
                @Index(
                        name = "idx_purchase_invoice_due_date",
                        columnList = "tenant_id,branch_id,due_date"
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
public class PurchaseInvoice extends BaseFinancialDocument {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @UuidGenerator
    private UUID id;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "supplier_id", nullable = false)
    private Supplier supplier;

    @Column(name = "supplier_invoice_number", nullable = false)
    private String supplierInvoiceNumber;

    @Column(nullable = false)
    private LocalDate dueDate;

    @Column(nullable = false, precision = 19, scale = 2)
    @Builder.Default
    private BigDecimal subtotal = BigDecimal.ZERO;

    @Column(nullable = false, precision = 19, scale = 2)
    @Builder.Default
    private BigDecimal vatAmount = BigDecimal.ZERO;

    @Column(nullable = false, precision = 19, scale = 2)
    @Builder.Default
    private BigDecimal discountAmount = BigDecimal.ZERO;

    @Column(nullable = false, precision = 19, scale = 2)
    @Builder.Default
    private BigDecimal totalAmount = BigDecimal.ZERO;

    @Column(nullable = false, precision = 19, scale = 2)
    @Builder.Default
    private BigDecimal allocatedAmount = BigDecimal.ZERO;

    @Column(nullable = false, precision = 19, scale = 2)
    @Builder.Default
    private BigDecimal outstandingAmount = BigDecimal.ZERO;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private PurchaseInvoiceStatus status;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private PurchaseInvoicePostingStatus postingLifecycleStatus;

    @Column(nullable = false)
    @Builder.Default
    private boolean overdue = false;

    @Column(nullable = false)
    @Builder.Default
    private boolean fullyAllocated = false;

    @Column(nullable = false)
    @Builder.Default
    private boolean cancelled = false;

    @Column(length = 4000)
    private String notes;

    @OneToMany(
            mappedBy = "purchaseInvoice",
            cascade = CascadeType.ALL,
            orphanRemoval = true
    )
    @Builder.Default
    private Set<PurchaseInvoiceLine> lines =
            new LinkedHashSet<>();
}