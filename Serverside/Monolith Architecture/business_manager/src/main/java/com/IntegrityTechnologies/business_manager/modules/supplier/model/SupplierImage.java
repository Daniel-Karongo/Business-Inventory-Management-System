package com.IntegrityTechnologies.business_manager.modules.supplier.model;

import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "supplier_images")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SupplierImage {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String fileName;
    private String filePath;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "supplier_id")
    private Supplier supplier;
}