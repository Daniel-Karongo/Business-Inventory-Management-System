package com.IntegrityTechnologies.business_manager.modules.platform.identity.entity;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "platform_user_sessions",
        indexes = {
                @Index(name = "idx_platform_session_user", columnList = "user_id"),
                @Index(name = "idx_platform_session_token", columnList = "token_id")
        }
)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PlatformUserSession {

    @Id
    @GeneratedValue
    private UUID id;

    @Column(nullable = false)
    private UUID userId;

    @Column(nullable = false)
    private UUID tokenId;

    private LocalDate loginDate;

    private LocalDateTime loginTime;

    private LocalDateTime logoutTime;

    private boolean autoLoggedOut;

    public void logout(LocalDateTime time, boolean auto) {
        this.logoutTime = time;
        this.autoLoggedOut = auto;
    }
}