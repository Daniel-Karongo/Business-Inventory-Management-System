package com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "user_sessions",
        indexes = {
                @Index(name = "idx_session_user_day", columnList = "tenant_id, user_id, login_date"),
                @Index(name = "idx_session_active", columnList = "tenant_id, user_id, logout_time")
        }
)
@Getter
@Setter
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public class UserSession extends BranchAwareEntity {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(name = "user_id", nullable = false, columnDefinition = "BINARY(16)")
    private UUID userId;

    /**
     * Calendar date of login (server timezone).
     */
    @Column(name = "login_date", nullable = false)
    private LocalDate loginDate;

    @Column(name = "login_time", nullable = false)
    private LocalDateTime loginTime;

    @Column(name = "logout_time")
    private LocalDateTime logoutTime;

    @Column(name = "auto_logged_out", nullable = false)
    private boolean autoLoggedOut;

    @Column(name = "token_id", columnDefinition = "BINARY(16)", nullable = false, unique = true)
    private UUID tokenId;

    /* ===================== HELPERS ===================== */

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