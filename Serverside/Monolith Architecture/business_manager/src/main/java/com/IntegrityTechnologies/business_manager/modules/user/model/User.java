package com.IntegrityTechnologies.business_manager.modules.user.model;

import jakarta.persistence.*;
import lombok.*;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Entity
@Table(name = "users")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class User {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)") // store as binary in MySQL
    private UUID id;

    @Column(nullable = false, unique = true)
    private String username;

    @Column(nullable = false)
    private String password;

    @Column(unique = true)
    private String emailAddress;

    @Column(unique = true)
    private String idNumber;

    @ElementCollection(fetch = FetchType.EAGER)
    @CollectionTable(name = "user_id_images", joinColumns = @JoinColumn(name = "user_id"))
    @Column(name = "image_url")
    private List<String> idImageUrls = new ArrayList<>();

    @Enumerated(EnumType.STRING)
    private Role role;

    private boolean deleted = false;
    private LocalDateTime deletedAt;

    private LocalDateTime createdAt;
    private LocalDateTime lastModifiedAt;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "created_by_id")
    private User createdBy;  // 👈 who created this user (nullable)

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "last_modified_by_id")
    private User lastModifiedBy;  // 👈 who last modified this user

    @PrePersist
    protected void onCreate() {
        this.createdAt = LocalDateTime.now();
        if (this.id == null) {
            this.id = UUID.randomUUID(); // generate UUID before insert
        }
    }
}
