package com.IntegrityTechnologies.business_manager.modules.supplier.model;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

@Entity
@Table(name = "supplier_image_audits")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SupplierImageAudit {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String fileName;
    private String filePath;
    private String action;
    private String reason;
    private LocalDateTime timestamp;
    private Long supplierId;
    private String supplierName;

    private String performedBy;
}