package com.IntegrityTechnologies.business_manager.modules.platform.tenant.model;

import com.IntegrityTechnologies.business_manager.security.util.TenantEntityListener;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.hibernate.annotations.Filter;
import org.hibernate.annotations.FilterDef;
import org.hibernate.annotations.ParamDef;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.time.LocalDateTime;
import java.util.UUID;

@MappedSuperclass
@EntityListeners(TenantEntityListener.class)
@FilterDef(
        name = "tenantFilter",
        parameters = @ParamDef(
                name = "tenantId",
                type = UUID.class
        )
)

@Filter(
        name = "tenantFilter",
        condition = "tenant_id = :tenantId"
)

@Getter
@Setter
@SuperBuilder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
public abstract class TenantAwareEntity {

    @Column(name = "tenant_id", nullable = false, updatable = false)
    private UUID tenantId;

    @CreationTimestamp
    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt;

    @Column(nullable = false)
    @Builder.Default
    private boolean deleted = false;

    @Column(name = "deleted_at")
    private LocalDateTime deletedAt;

    /*
     * Deleted because parent branch was deleted.
     * Allows restore without resurrecting
     * independently deleted records.
     */
    @Column(name = "branch_deleted", nullable = false)
    @Builder.Default
    private boolean branchDeleted = false;

    @Column(name = "branch_deleted_at")
    private LocalDateTime branchDeletedAt;
    @Version
    @Column(nullable = false)
    private Long version = 0L;

    @PrePersist
    protected void prePersist() {

        if (createdAt == null) {
            createdAt = LocalDateTime.now();
        }

        updatedAt = LocalDateTime.now();

        if (!deleted) {
            deleted = false;
        }

        if (version == null) {
            version = 0L;
        }

        beforePersist();
    }

    protected void beforePersist() {
        // default no-op
    }

    protected void beforeUpdate() {
        // default no-op
    }

    @PreUpdate
    protected void preUpdate() {
        updatedAt = LocalDateTime.now();
        beforeUpdate();
    }

    public void softDelete() {
        this.deleted = true;
        this.deletedAt = LocalDateTime.now();
    }

    public void restore() {
        this.deleted = false;
        this.deletedAt = null;
    }

    public void branchDelete() {
        this.deleted = true;
        this.branchDeleted = true;
        this.deletedAt = LocalDateTime.now();
        this.branchDeletedAt = LocalDateTime.now();
    }

    public void restoreFromBranchDelete() {
        if (!branchDeleted) {
            return;
        }

        this.deleted = false;
        this.deletedAt = null;
        this.branchDeleted = false;
        this.branchDeletedAt = null;
    }
}