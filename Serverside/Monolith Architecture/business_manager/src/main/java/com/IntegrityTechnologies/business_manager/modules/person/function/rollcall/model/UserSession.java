package com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "user_sessions")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserSession {
    @Id
    @GeneratedValue
    private UUID id;

    @Column(nullable = false)
    private UUID userId;

    @Column(nullable = false)
    private UUID branchId;   // ✅ add this

    private LocalDateTime loginTime;
    private LocalDateTime logoutTime;

    private Boolean autoLoggedOut = false;
}