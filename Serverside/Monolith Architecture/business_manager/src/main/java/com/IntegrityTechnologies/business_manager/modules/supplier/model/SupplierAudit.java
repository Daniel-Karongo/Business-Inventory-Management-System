package com.IntegrityTechnologies.business_manager.modules.supplier.model;

import com.IntegrityTechnologies.business_manager.modules.user.model.User;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

@Entity
@Table(name = "supplier_audit")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SupplierAudit {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String action; // CREATED, UPDATED, DELETED, RESTORED
    private String fieldChanged; // optional — name, email, rating, etc.
    private String oldValue;
    private String newValue;

    private String reason; // optional
    private LocalDateTime timestamp;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "supplier_id")
    private Supplier supplier;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "performed_by")
    private User performedBy;
}