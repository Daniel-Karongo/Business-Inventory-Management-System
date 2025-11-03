package com.IntegrityTechnologies.business_manager.modules.supplier.model;

import com.IntegrityTechnologies.business_manager.modules.user.model.User;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

@Entity
@Table(name = "supplier_image_audit")
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

    private String action; // "DELETED", "UPLOADED", etc.
    private String reason; // optional: "User deleted manually" or "Batch cleanup"

    private LocalDateTime timestamp;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "supplier_id")
    private Supplier supplier;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "performed_by")
    private User performedBy;
}