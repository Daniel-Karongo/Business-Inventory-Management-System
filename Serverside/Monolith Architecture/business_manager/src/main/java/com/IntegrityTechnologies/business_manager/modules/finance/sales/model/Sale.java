package com.IntegrityTechnologies.business_manager.modules.finance.sales.model;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.model.Payment;
import jakarta.persistence.*;
import lombok.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
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
    @Column(unique = true)
    private String receiptNo;
    private LocalDateTime createdAt;

    private String createdBy;

    private BigDecimal totalAmount;

    private BigDecimal totalTax;

    private BigDecimal totalDiscount;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinColumn(name = "sale_id")
    @Builder.Default
    private List<SaleLineItem> lineItems = new ArrayList<>();

    @OneToMany(mappedBy = "sale", cascade = CascadeType.ALL, orphanRemoval = true)
    @Builder.Default
    private List<Payment> payments = new ArrayList<>();


    @Column(columnDefinition = "BINARY(16)")
    private UUID customerId;

    @Enumerated(EnumType.STRING)
    private SaleStatus status;

    @Version
    private Long version;

    public enum SaleStatus {
        CREATED, COMPLETED, CANCELLED, REFUNDED
    }
    @Column(precision = 19, scale = 2)
    private BigDecimal costOfGoodsSold;
}