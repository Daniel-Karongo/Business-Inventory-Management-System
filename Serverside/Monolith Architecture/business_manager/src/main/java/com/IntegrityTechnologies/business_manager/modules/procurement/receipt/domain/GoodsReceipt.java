package com.IntegrityTechnologies.business_manager.modules.procurement.receipt.domain;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.Check;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "goods_receipts",
        indexes = {
                @Index(
                        name = "idx_grn_branch",
                        columnList = "tenant_id,branch_id"
                ),
                @Index(
                        name = "idx_grn_supplier",
                        columnList = "supplier_id"
                ),
                @Index(
                        name = "idx_grn_match_status",
                        columnList = "tenant_id,branch_id,invoiced"
                )
        },
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_grn_tenant_branch_receipt",
                        columnNames = {
                                "tenant_id",
                                "branch_id",
                                "receiptNumber"
                        }
                )
        }
)
@Check(
        constraints =
                "ABS(gross_amount - (net_amount + vat_amount)) <= 0.01"
)
@Getter
@Setter
@NoArgsConstructor
public class GoodsReceipt
        extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false)
    private String receiptNumber;

    @Column(nullable = false)
    private UUID supplierId;

    @Column(nullable = false)
    private UUID productId;

    @Column(nullable = false)
    private UUID variantId;

    @Column(nullable = false)
    private Long quantity;

    @Column(nullable = false, precision = 19, scale = 6)
    private BigDecimal unitCost;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal netAmount;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal vatAmount;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal grossAmount;

    @Column(nullable = false)
    private Boolean vatInclusive;

    @Column(nullable = false, precision = 19, scale = 6)
    private BigDecimal vatRate;

    @Column(nullable = false)
    private LocalDate receiptDate;

    private String supplierReference;

    private String note;

    @Column(nullable = false)
    private boolean invoiced = false;

    private UUID matchedInvoiceId;

    private LocalDateTime invoicedAt;

    @Column(nullable = false)
    private boolean reversed = false;

    private LocalDateTime reversedAt;

    private String reversedBy;

    private String reversalReason;

    @Override
    protected void beforePersist() {
        normalizeMoney();
    }

    @Override
    protected void beforeUpdate() {
        normalizeMoney();
    }

    private void normalizeMoney() {

        if (netAmount != null) {
            netAmount =
                    netAmount.setScale(
                            2,
                            BigDecimal.ROUND_HALF_UP
                    );
        }

        if (vatAmount != null) {
            vatAmount =
                    vatAmount.setScale(
                            2,
                            BigDecimal.ROUND_HALF_UP
                    );
        }

        if (grossAmount != null) {
            grossAmount =
                    grossAmount.setScale(
                            2,
                            BigDecimal.ROUND_HALF_UP
                    );
        }

        if (unitCost != null) {
            unitCost =
                    unitCost.setScale(
                            6,
                            BigDecimal.ROUND_HALF_UP
                    );
        }
    }
}