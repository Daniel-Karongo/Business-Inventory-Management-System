package com.IntegrityTechnologies.business_manager.modules.person.entity.user.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.AuditableTenantEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.hibernate.annotations.JdbcTypeCode;

import java.sql.Types;
import java.time.LocalDateTime;
import java.util.*;

@Entity
@Table(
        name = "users",
        indexes = {

                @Index(
                        name = "idx_user_tenant_username",
                        columnList = "tenant_id, username"
                ),

                @Index(
                        name = "idx_user_tenant_role",
                        columnList = "tenant_id, role"
                ),

                @Index(
                        name = "idx_user_tenant_deleted",
                        columnList = "tenant_id, deleted"
                )

        },

        uniqueConstraints = {

                @UniqueConstraint(
                        name = "uq_user_tenant_username",
                        columnNames = {"tenant_id", "username"}
                )

        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
public class User extends AuditableTenantEntity {

    @Id
    @GeneratedValue
    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(nullable = false)
    private String username;

    @Column(nullable = false)
    private String password;

    @ElementCollection
    @CollectionTable(
            name = "user_email_addresses",
            joinColumns = @JoinColumn(name = "user_id")
    )
    @Column(name = "email")
    private List<String> emailAddresses = new ArrayList<>();

    @ElementCollection
    @CollectionTable(
            name = "user_phone_numbers",
            joinColumns = @JoinColumn(name = "user_id")
    )
    @Column(name = "phone")
    private List<String> phoneNumbers = new ArrayList<>();

    private String idNumber;

    @Enumerated(EnumType.STRING)
    private Role role;

    @Column(nullable = false)
    private String uploadFolder;

    private Boolean deleted = false;

    private LocalDateTime deletedAt;

    /* ================================
       RELATIONSHIPS
    ================================= */

    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<UserBranch> branches = new HashSet<>();

    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<UserDepartment> departments = new HashSet<>();

    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<UserImage> images = new ArrayList<>();

}