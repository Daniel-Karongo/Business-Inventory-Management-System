package com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.model;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "mpesa_transactions",
        indexes = {
            @Index(name = "idx_mpesa_cb", columnList = "checkoutRequestId"),
            @Index(name = "idx_mpesa_ref", columnList = "mpesaReceiptNumber")
        },
        uniqueConstraints = {
            @UniqueConstraint(columnNames = "checkoutRequestId")
})
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class MpesaTransaction {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    private String checkoutRequestId;
    private String merchantRequestId;
    private String mpesaReceiptNumber; // receipt
    private UUID saleId; // link to sale (optional)
    private BigDecimal amount;
    private String phoneNumber;
    private String status; // PENDING, SUCCESS, FAILED
    private LocalDateTime timestamp;
    @Column(length = 500)
    private String rawResponse;
}