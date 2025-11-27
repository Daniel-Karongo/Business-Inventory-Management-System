package com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.JdbcTypeCode;

import java.sql.Types;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "supplier_image_audits")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SupplierImageAudit {

    @Id
    @GeneratedValue
    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID supplierId;
    private String supplierName;

    private String fileName;
    private String filePath;
    private String action;
    private String reason;
    private String performedBy;
    private LocalDateTime timestamp;
}