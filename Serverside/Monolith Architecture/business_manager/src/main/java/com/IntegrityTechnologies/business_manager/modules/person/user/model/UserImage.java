package com.IntegrityTechnologies.business_manager.modules.person.user.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.TenantAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.hibernate.annotations.JdbcTypeCode;

import java.sql.Types;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "user_images",
        indexes = {

                @Index(name = "idx_user_image_tenant",
                        columnList = "tenant_id"),

                @Index(name = "idx_user_image_user",
                        columnList = "user_id"),

                @Index(name = "idx_user_image_deleted",
                        columnList = "deleted"),

                @Index(name = "idx_user_thumbnail_lookup", columnList = "user_id, profile_thumbnail, deleted")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class UserImage extends TenantAwareEntity {

    @Id
    @GeneratedValue
    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(nullable = false)
    private String fileName;

    @Column(nullable = false)
    private String filePath;

    @Column(nullable = false)
    private String fileDescription;

    @Column(nullable = false)
    @Builder.Default
    private Boolean profileThumbnail = false;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", nullable = false)
    @ToString.Exclude
    @EqualsAndHashCode.Exclude
    private User user;

    private LocalDateTime deletedAt;

    @Column(nullable = false)
    @Builder.Default
    private Boolean deleted = false;

    private LocalDateTime uploadedAt;

    @Override
    protected void beforePersist() {

        if (profileThumbnail == null) {
            profileThumbnail = false;
        }

    }
}