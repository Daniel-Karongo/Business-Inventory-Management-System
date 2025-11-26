package com.IntegrityTechnologies.business_manager.modules.biometric.model;

import com.IntegrityTechnologies.business_manager.modules.user.model.User;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "biometric_records")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BiometricRecord {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", nullable = false)
    private User user;

    // Type: FINGERPRINT, IRIS, FACE etc.
    @Enumerated(EnumType.STRING)
    private BiometricType type;

    /**
     * Provider ID / external reference (if using vendor). Not sensitive.
     */
    private String providerId;

    /**
     * A safe hash / template ID — NOT raw image. This is used for local comparisons if you keep templates.
     * For security, store only a salted hash or template token.
     */
    private String templateHash;

    private LocalDateTime enrolledAt;

    private boolean deleted = false;
}