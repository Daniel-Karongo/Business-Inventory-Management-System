package com.IntegrityTechnologies.business_manager.modules.finance.tax.domain;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "vat_filings")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class VatFiling {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @ManyToOne(optional = false)
    private TaxPeriod period;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal outputVat;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal inputVat;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal vatPayable;

    private String filedBy;
    private LocalDateTime filedAt;

    private boolean paid;
}