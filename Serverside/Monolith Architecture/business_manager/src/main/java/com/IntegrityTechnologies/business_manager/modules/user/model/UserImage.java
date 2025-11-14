package com.IntegrityTechnologies.business_manager.modules.user.model;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.JdbcTypeCode;
import java.sql.Types;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "user_images", indexes = {@Index(name = "idx_user_image_user", columnList = "user_id")})
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserImage {

    @Id
    @GeneratedValue
    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(nullable = false)
    private String fileName;

    @Column(nullable = false)
    private String filePath;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", nullable = false)
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private User user;

    private LocalDateTime deletedAt;
    private Boolean deleted;
    private LocalDateTime uploadedAt;

    @PrePersist
    public void onCreate() {
        uploadedAt = LocalDateTime.now();
        deleted = false;
    }
}