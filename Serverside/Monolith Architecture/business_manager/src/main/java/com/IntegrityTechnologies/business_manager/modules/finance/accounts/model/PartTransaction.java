package com.IntegrityTechnologies.business_manager.modules.finance.accounts.model;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "part_transactions", indexes = {
        @Index(name = "idx_parttrans_ref", columnList = "referenceId"),
        @Index(name = "idx_parttrans_module", columnList = "relatedModule")
})
@Data
@NoArgsConstructor
@AllArgsConstructor
public class PartTransaction {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(nullable = false)
    private String relatedModule;

    @Column(columnDefinition = "BINARY(16)")
    private UUID referenceId;

    @Column(columnDefinition = "BINARY(16)")
    private UUID productId;

    private Integer quantity;

    @Column(precision = 19, scale = 2)
    private BigDecimal unitPrice;

    @Column(precision = 19, scale = 2)
    private BigDecimal transactionTotal;

    private LocalDateTime transactionDate;

    private String createdBy;

    private String note;

    @Version
    private Long version;
}