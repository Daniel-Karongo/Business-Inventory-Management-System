package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.JdbcTypeCode;

import java.sql.Types;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "product_images",
        indexes = { @Index(name = "idx_product_image_product_id", columnList = "product_id") })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ProductImage {

    @Id
    @GeneratedValue
    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    private String fileName;
    private String filePath;
    private String thumbnailFileName;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "product_id")
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private Product product;
    @Column(nullable = false)
    private Boolean primaryImage = false;

    @Column(nullable = false)
    private Boolean deleted = false;


    private LocalDateTime uploadedAt;

    @PrePersist
    public void onCreate() {
        if (uploadedAt == null)
            uploadedAt = LocalDateTime.now();
        if (deleted == null)
            deleted = false;
        if (primaryImage == null)
            primaryImage = false;
    }

}