package com.IntegrityTechnologies.business_manager.modules.finance.tax.domain;

import jakarta.persistence.*;
import lombok.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CorporateTaxFiling {

    @Id
    @GeneratedValue
    private UUID id;

    private UUID periodId;

    private BigDecimal taxableProfit;

    private BigDecimal taxRate;

    private BigDecimal taxAmount;

    private boolean paid;

    private String filedBy;

    private LocalDateTime filedAt;

    private LocalDateTime paidAt;
}