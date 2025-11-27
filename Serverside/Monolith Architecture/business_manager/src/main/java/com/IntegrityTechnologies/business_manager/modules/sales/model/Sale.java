package com.IntegrityTechnologies.business_manager.modules.sales.model;

import jakarta.persistence.*;
import lombok.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Entity
@Table(name = "sales")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Sale {

    @Id
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    private LocalDateTime createdAt;

    private String createdBy;

    private BigDecimal totalAmount;

    private BigDecimal totalTax;

    private BigDecimal totalDiscount;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinColumn(name = "sale_id")
    private List<SaleLineItem> lineItems;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinColumn(name = "sale_id")
    private List<PaymentRecord> payments;

    // status: CREATED, COMPLETED, CANCELLED, REFUNDED
    @Enumerated(EnumType.STRING)
    private SaleStatus status;

    public enum SaleStatus {
        CREATED, COMPLETED, CANCELLED, REFUNDED
    }
}