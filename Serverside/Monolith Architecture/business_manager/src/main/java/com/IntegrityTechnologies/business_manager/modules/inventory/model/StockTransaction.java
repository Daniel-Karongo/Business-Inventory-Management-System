package com.IntegrityTechnologies.business_manager.modules.inventory.model;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "stock_transactions", indexes = {@Index(name = "idx_stock_tx_product", columnList = "product_id")})
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class StockTransaction {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private UUID id;

    // product reference (denormalized for faster reporting)
    @Column(name = "product_id", nullable = false)
    private UUID productId;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private TransactionType type;

    @Column(nullable = false)
    private Long quantityDelta; // positive for receipts, negative for consumption

    private String reference; // e.g., saleId, purchaseOrderId, adjustmentId

    private String note;

    private LocalDateTime timestamp;

    private String performedBy;

    public enum TransactionType {
        RECEIPT, // goods received from supplier
        SALE,    // sale consumed inventory
        ADJUSTMENT, // manual stock correction
        TRANSFER_IN,
        TRANSFER_OUT,
        RESERVATION, // reserved for order
        RELEASE // release reservation
    }
}