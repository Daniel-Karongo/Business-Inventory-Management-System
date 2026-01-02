package com.IntegrityTechnologies.business_manager.modules.person.function.auth.model;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "password_reset_tokens",
        indexes = {
                @Index(name = "idx_prt_user", columnList = "user_id"),
                @Index(name = "idx_prt_token", columnList = "token_hash")
        }
)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class PasswordResetToken {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(columnDefinition = "BINARY(16)", nullable = false)
    private UUID userId;

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

    @PrePersist
    void onCreate() {
        createdAt = LocalDateTime.now();
        used = false;
        attempts = 0;
    }
}