package com.IntegrityTechnologies.business_manager.modules.supplier.model;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import java.time.LocalDateTime;

@Entity
@Table(name = "supplier_images", indexes = {@Index(name = "idx_supplier_image_supplier", columnList = "supplier_id")})
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SupplierImage {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private String fileName;

    /**
     * Internal absolute filesystem path. Do NOT expose this to clients.
     */
    @Column(nullable = false)
    private String filePath;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "supplier_id", nullable = false)
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private Supplier supplier;

    @SQLDelete(sql = "UPDATE supplier_images SET deleted = true WHERE id = ?")
    @Where(clause = "deleted = false")
    private boolean deleted = false;

    private LocalDateTime uploadedAt;

    @PrePersist
    public void onCreate() {
        uploadedAt = LocalDateTime.now();
    }
}