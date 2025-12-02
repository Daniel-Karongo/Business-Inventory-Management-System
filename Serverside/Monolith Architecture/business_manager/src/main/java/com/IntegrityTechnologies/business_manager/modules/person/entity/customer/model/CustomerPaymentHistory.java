package com.IntegrityTechnologies.business_manager.modules.person.entity.customer.model;

import jakarta.persistence.*;
import lombok.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "customer_payment_history", indexes = {
        @Index(name = "idx_cph_customer", columnList = "customerId")
})
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CustomerPaymentHistory {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(columnDefinition = "BINARY(16)")
    private UUID customerId;

    @Column(columnDefinition = "BINARY(16)")
    private UUID paymentId;

    private BigDecimal amount;

    private LocalDateTime timestamp;

    private String note;
}