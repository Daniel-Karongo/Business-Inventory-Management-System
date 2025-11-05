package com.IntegrityTechnologies.business_manager.modules.supplier.model;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

@Entity
@Table(name = "supplier_audits")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SupplierAudit {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String action;
    private String fieldChanged;
    @Column(length = 2000)
    private String oldValue;
    @Column(length = 2000)
    private String newValue;
    private String reason;
    private LocalDateTime timestamp;
    private Long supplierId;
    private String supplierName;

    private String performedBy;
}