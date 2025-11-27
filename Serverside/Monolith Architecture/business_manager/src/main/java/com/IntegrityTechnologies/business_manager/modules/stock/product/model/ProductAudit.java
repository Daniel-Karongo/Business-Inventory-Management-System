package com.IntegrityTechnologies.business_manager.modules.stock.product.model;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.JdbcTypeCode;

import java.sql.Types;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "product_audits")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ProductAudit {
    @Id
    @GeneratedValue
    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    private String action;
    private String fieldChanged;

    @Column(length = 2000)
    private String oldValue;

    @Column(length = 2000)
    private String newValue;

    private String reason;
    private LocalDateTime timestamp;

    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID productId;

    private String productName;

    private String performedBy;
}