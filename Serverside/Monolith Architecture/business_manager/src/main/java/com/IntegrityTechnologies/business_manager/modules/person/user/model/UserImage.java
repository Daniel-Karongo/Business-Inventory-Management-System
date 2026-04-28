package com.IntegrityTechnologies.business_manager.modules.person.user.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
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

                @Index(name = "idx_user_image_tenant_branch",
                        columnList = "tenant_id, branch_id"),

                @Index(name = "idx_user_image_user",
                        columnList = "user_id"),

                @Index(name = "idx_user_image_deleted",
                        columnList = "deleted")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class UserImage extends BranchAwareEntity {

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

    @PrePersist
    public void onCreate() {

        if (uploadedAt == null) {
            uploadedAt = LocalDateTime.now();
        }

        if (deleted == null) {
            deleted = false;
        }

        if (getBranchId() == null) {
            throw new IllegalStateException(
                    "branchId must be assigned before persisting UserImage"
            );
        }
    }
}