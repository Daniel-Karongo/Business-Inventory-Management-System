package com.IntegrityTechnologies.business_manager.modules.person.entity.user.model;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.JdbcTypeCode;
import java.sql.Types;
import java.time.LocalDateTime;
import java.util.*;

@Entity
@Table(name = "users",
        indexes = {
                @Index(name = "idx_user_username", columnList = "username")
        })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class User {

    @Id
    @GeneratedValue
    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(nullable = false, unique = true)
    private String username;

    @Column(nullable = false)
    private String password;

    @ElementCollection
    @CollectionTable(
            name = "user_email_addresses",
            joinColumns = @JoinColumn(name = "user_id"),
            indexes = {
                    @Index(name = "idx_email_unique", columnList = "email_address")
            }
    )
    @Column(name = "email_address")
    private List<String> emailAddresses = new ArrayList<>();

    @ElementCollection
    @CollectionTable(name = "user_phone_numbers", joinColumns = @JoinColumn(name = "user_id"))
    @Column(name = "phone_number")
    private List<String> phoneNumbers = new ArrayList<>();

    @Column(unique = true)
    private String idNumber;

    @Enumerated(EnumType.STRING)
    private Role role;

    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @Builder.Default
    private List<UserImage> images = new ArrayList<>();
    @Column(nullable = false, unique = true)
    private String uploadFolder;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "created_by_id")
    private User createdBy;

    private LocalDateTime createdAt;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "updated_by_id")
    private User updatedBy;

    private LocalDateTime updatedAt;

    private LocalDateTime deletedAt;
    private Boolean deleted;

    @PrePersist
    public void onCreate() {
        createdAt = LocalDateTime.now();
        deleted = false;
    }

    @PreUpdate
    public void onUpdate() {
        updatedAt = LocalDateTime.now();
    }
}