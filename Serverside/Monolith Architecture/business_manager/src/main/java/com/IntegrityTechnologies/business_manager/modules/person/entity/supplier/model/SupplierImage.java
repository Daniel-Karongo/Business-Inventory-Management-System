package com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.JdbcTypeCode;

import java.sql.Types;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "supplier_images", indexes = {@Index(name = "idx_supplier_image_supplier", columnList = "supplier_id")})
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SupplierImage {

    @Id
    @GeneratedValue
    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(nullable = false)
    private String fileName;

    /**
     * Internal absolute filesystem path. Do NOT expose this to clients.
     */
    @Column(nullable = false)
    private String filePath;

    @Column(nullable = false)
    private String fileDescription;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "supplier_id", nullable = false)
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private Supplier supplier;

    private LocalDateTime deletedAt;
    private Boolean deleted;

    private LocalDateTime uploadedAt;

    @PrePersist
    public void onCreate() {
        uploadedAt = LocalDateTime.now();
        deleted = false;
    }
}