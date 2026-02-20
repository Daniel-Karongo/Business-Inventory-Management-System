package com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "accounting_periods",
        uniqueConstraints = @UniqueConstraint(
                columnNames = {"startDate", "endDate"}
        ))
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AccountingPeriod {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    private LocalDate startDate;
    private LocalDate endDate;

    private boolean closed;

    // 🔥 ADD THESE
    private boolean taxAccrued;

    private LocalDateTime taxAccruedAt;
}