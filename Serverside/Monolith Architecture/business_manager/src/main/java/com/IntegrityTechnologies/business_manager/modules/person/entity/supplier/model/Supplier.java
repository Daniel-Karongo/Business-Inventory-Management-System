package com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.JdbcTypeCode;

import java.sql.Types;
import java.time.LocalDateTime;
import java.util.*;

@Entity
@Table(name = "suppliers",
        indexes = {
                @Index(name = "idx_supplier_name", columnList = "name"),
                @Index(name = "idx_supplier_region", columnList = "region")
        })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@ToString
public class Supplier {

    @Id
    @GeneratedValue
    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    @ToString.Exclude
    private UUID id;

    @Column(nullable = false, unique = true)
    private String name;

    @ElementCollection
    @CollectionTable(name = "supplier_emails", joinColumns = @JoinColumn(name = "supplier_id"))
    @Column(name = "email")
    private Set<String> email = new HashSet<>();

    @ElementCollection
    @CollectionTable(name = "supplier_phone_numbers", joinColumns = @JoinColumn(name = "supplier_id"))
    @Column(name = "phone_number")
    private Set<String> phoneNumber = new HashSet<>();

    private String address;
    private String region;
    private Double rating;

    @OneToMany(mappedBy = "supplier", cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @Builder.Default
    private Set<SupplierImage> images = new HashSet<>();

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