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
                ),

                @Index(
                        name = "idx_user_tenant_idnumber",
                        columnList = "tenant_id, id_number"
                )
        },
        uniqueConstraints = {

                @UniqueConstraint(
                        name = "uq_user_tenant_username",
                        columnNames = {"tenant_id", "username"}
                ),

                @UniqueConstraint(
                        name = "uq_user_tenant_idnumber",
                        columnNames = {"tenant_id", "id_number"}
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

    /* =====================================
       EMAIL ADDRESSES
    ===================================== */

    @ElementCollection
    @CollectionTable(
            name = "user_email_addresses",
            joinColumns = @JoinColumn(name = "user_id"),
            uniqueConstraints = {
                    @UniqueConstraint(
                            name = "uq_user_email",
                            columnNames = {"user_id", "email"}
                    )
            },
            indexes = {
                    @Index(name = "idx_user_email", columnList = "email")
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
            name = "user_phone_numbers",
            joinColumns = @JoinColumn(name = "user_id"),
            uniqueConstraints = {
                    @UniqueConstraint(
                            name = "uq_user_phone",
                            columnNames = {"user_id", "phone"}
                    )
            },
            indexes = {
                    @Index(name = "idx_user_phone", columnList = "phone")
            }
    )
    @Column(name = "phone")
    @Builder.Default
    private List<String> phoneNumbers = new ArrayList<>();


    /* =====================================
       BASIC INFO
    ===================================== */

    private String idNumber;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private Role role;

    @Column(nullable = false)
    private String uploadFolder;

    @Column(nullable = false)
    @Builder.Default
    private Boolean deleted = false;

    private LocalDateTime deletedAt;


    /* =====================================
       RELATIONSHIPS
    ===================================== */

    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
    @Builder.Default
    private Set<UserBranch> branches = new HashSet<>();


    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
    @Builder.Default
    private Set<UserDepartment> departments = new HashSet<>();


    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
    @Builder.Default
    private List<UserImage> images = new ArrayList<>();

    @Column(nullable = false)
    @Builder.Default
    private Boolean mustChangePassword = true;
}