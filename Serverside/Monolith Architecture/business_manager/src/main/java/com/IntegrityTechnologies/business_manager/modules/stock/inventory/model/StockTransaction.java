package com.IntegrityTechnologies.business_manager.modules.stock.inventory.model;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.JdbcTypeCode;

import java.math.BigDecimal;
import java.sql.Types;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "stock_transactions", indexes = {
        @Index(name = "idx_stock_tx_product", columnList = "product_id"),
        @Index(name = "idx_stock_tx_deleted", columnList = "deleted")
})
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class StockTransaction {

    @Id
    @GeneratedValue
    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    // product reference (denormalized for faster reporting)
    @Column(name = "product_id", nullable = false)
    private UUID productId;

    @Column(name = "product_variant_id", nullable = false)
    private UUID productVariantId;


    @Column(name = "branch_id", nullable = false)
    private UUID branchId;

    @Column(precision = 18, scale = 2)
    private BigDecimal unitCost;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private TransactionType type;

    @Column(name = "supplier_id")
    private UUID supplierId;

    @Column(nullable = false)
    private Long quantityDelta; // positive for receipts, negative for consumption

    private String reference; // e.g., saleId, purchaseOrderId, adjustmentId

    private String note;

    private LocalDateTime timestamp;

    private String performedBy;

    @Column(nullable = false)
    private Boolean deleted = false;

    public enum TransactionType {
        RECEIPT, // goods received from supplier
        SALE,    // sale consumed inventory
        ADJUSTMENT, // manual stock correction
        TRANSFER_IN,
        TRANSFER_OUT,
        RESERVATION, // reserved for order
        RELEASE, // release reservation
        RETURN
    }

    @Version
    @Column(name = "version", nullable = false)
    private Long version;
}