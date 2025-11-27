package com.IntegrityTechnologies.business_manager.modules.finance.payment.model;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.Sale;
import jakarta.persistence.*;
import lombok.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "payments")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Payment {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "sale_id", nullable = false)
    private Sale sale;

    @Column(nullable = false)
    private BigDecimal amount;

    @Column(nullable = false)
    private String method; // e.g. CASH, MPESA, CARD, EFT

    private String providerReference; // e.g. transaction id from MPESA or card gateway

    @Column(nullable = false)
    private LocalDateTime timestamp;

    @Column(nullable = false)
    private String status; // e.g. PENDING, SUCCESS, FAILED

    private String note;
}