package com.IntegrityTechnologies.business_manager.modules.platform.identity.entity;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.UuidGenerator;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Entity
@Table(
        name = "platform_users",
        indexes = {
                @Index(name = "idx_platform_user_username", columnList = "username"),
                @Index(name = "idx_platform_user_role", columnList = "role"),
                @Index(name = "idx_platform_user_active", columnList = "active"),
                @Index(name = "idx_platform_user_deleted", columnList = "deleted")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class PlatformUser {

    @Id
    @GeneratedValue
    @UuidGenerator
    private UUID id;

    @Column(nullable = false, unique = true)
    private String username;

    @Column(nullable = false)
    private String password;

    @Column(nullable = false)
    @Builder.Default
    private Boolean mustChangePassword = true;

    /* =====================================
   EMAIL ADDRESSES
===================================== */

    @ElementCollection
    @CollectionTable(
            name = "platform_user_email_addresses",
            joinColumns = @JoinColumn(name = "platform_user_id"),
            uniqueConstraints = {
                    @UniqueConstraint(
                            name = "uq_platform_user_email",
                            columnNames = {"platform_user_id", "email"}
                    )
            },
            indexes = {
                    @Index(name = "idx_platform_user_email", columnList = "email")
            }
    )
    @Column(name = "email")
    @Builder.Default
    private List<String> emailAddresses = new ArrayList<>();


/* =====================================
   PHONE NUMBERS
===================================== */

    @ElementCollection
    @CollectionTable(
            name = "platform_user_phone_numbers",
            joinColumns = @JoinColumn(name = "platform_user_id"),
            uniqueConstraints = {
                    @UniqueConstraint(
                            name = "uq_platform_user_phone",
                            columnNames = {"platform_user_id", "phone"}
                    )
            },
            indexes = {
                    @Index(name = "idx_platform_user_phone", columnList = "phone")
            }
    )
    @Column(name = "phone")
    @Builder.Default
    private List<String> phoneNumbers = new ArrayList<>();


    /* =====================================
       IDENTITY
    ===================================== */

    @Column
    private String idNumber;

    @Column(nullable = false)
    private boolean active;

    @Column(nullable = false)
    private boolean locked;

    @Column(nullable = false)
    private boolean deleted;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private PlatformRole role;

    private LocalDateTime createdAt;

    private LocalDateTime updatedAt;

    @PrePersist
    public void prePersist() {

        createdAt = LocalDateTime.now();

        if (active == false) {
            active = true;
        }

        if (locked == false) {
            locked = false;
        }

        if (deleted == false) {
            deleted = false;
        }
    }

    @PreUpdate
    public void preUpdate() {
        updatedAt = LocalDateTime.now();
    }

}