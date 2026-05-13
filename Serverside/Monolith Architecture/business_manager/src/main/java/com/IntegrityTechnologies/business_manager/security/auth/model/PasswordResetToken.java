package com.IntegrityTechnologies.business_manager.security.auth.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.TenantAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "password_reset_tokens",
        indexes = {
                @Index(name = "idx_prt_tenant_branch_user", columnList = "tenant_id, branch_id, user_id"),
                @Index(name = "idx_prt_user", columnList = "user_id"),
                @Index(name = "idx_prt_token", columnList = "token_hash")
        }
)
@Data
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class PasswordResetToken extends TenantAwareEntity {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(columnDefinition = "BINARY(16)", nullable = false)
    private UUID userId;

    private UUID branchId;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private UserType userType;

    @Column(nullable = false, length = 128)
    private String tokenHash;

    @Enumerated(EnumType.STRING)
    private Channel channel;

    private LocalDateTime expiresAt;

    private int attempts;

    private boolean used;

    private LocalDateTime createdAt;

    public void incrementAttempts() {
        this.attempts++;
    }

    public void markUsed() {
        this.used = true;
    }

    public enum Channel {
        EMAIL, SMS
    }

    @Override
    public void beforePersist() {
        createdAt = LocalDateTime.now();
        used = false;
        attempts = 0;
    }
}