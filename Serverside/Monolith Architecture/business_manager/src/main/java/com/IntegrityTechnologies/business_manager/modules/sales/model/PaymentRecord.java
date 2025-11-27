package com.IntegrityTechnologies.business_manager.modules.sales.model;

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
public class PaymentRecord {

    @Id
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    private LocalDateTime paidAt;

    private BigDecimal amount;

    private String method; // CASH, MPESA, CARD, EFT

    private String reference; // gateway reference / transaction id

    private String status; // PENDING, COMPLETED, FAILED

    private String note;
}