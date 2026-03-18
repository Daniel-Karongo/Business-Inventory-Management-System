package com.IntegrityTechnologies.business_manager.modules.stock.inventory.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.hibernate.annotations.JdbcTypeCode;

import java.math.BigDecimal;
import java.sql.Types;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "stock_transactions", indexes = {
        @Index(name = "idx_stock_tx_tenant_branch", columnList = "tenant_id, branch_id"),
        @Index(name = "idx_stock_tx_product", columnList = "product_id"),
        @Index(name = "idx_stock_tx_deleted", columnList = "deleted")
})
@Data
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class StockTransaction extends BranchAwareEntity {

    @Id
    @GeneratedValue
    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(name = "product_id", nullable = false)
    private UUID productId;

    @Column(name = "product_variant_id", nullable = false)
    private UUID productVariantId;

    @Column(precision = 18, scale = 2)
    private BigDecimal unitCost;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private TransactionType type;

    @Column(name = "supplier_id")
    private UUID supplierId;

    @Column(nullable = false)
    private Long quantityDelta;

    private String reference;
    private String note;

    @Column(nullable = false, updatable = false)
    private LocalDateTime timestamp;

    private String performedBy;

    @Builder.Default
    @Column(nullable = false)
    private Boolean deleted = false;

    @Version
    private Long version;

    public enum TransactionType {
        RECEIPT,
        SALE,
        ADJUSTMENT,
        TRANSFER_IN,
        TRANSFER_OUT,
        RESERVATION,
        RELEASE,
        RETURN
    }

    @PrePersist
    public void onCreate() {
        if (timestamp == null) {
            timestamp = LocalDateTime.now();
        }
    }
}