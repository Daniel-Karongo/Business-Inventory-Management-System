package com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "user_sessions",
        indexes = {
                @Index(name = "idx_session_user_day", columnList = "user_id, login_date"),
                @Index(name = "idx_session_active", columnList = "user_id, logout_time")
        }
)
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UserSession {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(name = "user_id", nullable = false, columnDefinition = "BINARY(16)")
    private UUID userId;

    @Column(name = "branch_id", nullable = false, columnDefinition = "BINARY(16)")
    private UUID branchId;

    /**
     * Calendar date of login (server timezone).
     * Used for:
     * - session caps per day
     * - midnight expiry handling
     */
    @Column(name = "login_date", nullable = false)
    private LocalDate loginDate;

    @Column(name = "login_time", nullable = false)
    private LocalDateTime loginTime;

    @Column(name = "logout_time")
    private LocalDateTime logoutTime;

    /**
     * True if the session ended automatically
     * (idle timeout or token expiry)
     */
    @Column(name = "auto_logged_out", nullable = false)
    private boolean autoLoggedOut;

    /**
     * Optional but strongly recommended:
     * Unique ID embedded into JWT (jti)
     * Enables precise session validation.
     */
    @Column(name = "token_id", columnDefinition = "BINARY(16)", nullable = false, unique = true)
    private UUID tokenId;

    /* =====================
       Convenience helpers
       ===================== */

    @Transient
    public boolean isActive() {
        return logoutTime == null;
    }

    public void logout(LocalDateTime when, boolean auto) {
        if (this.logoutTime == null) {
            this.logoutTime = when;
            this.autoLoggedOut = auto;
        }
    }
}