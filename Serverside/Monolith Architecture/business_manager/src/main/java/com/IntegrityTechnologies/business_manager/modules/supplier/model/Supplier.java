package com.IntegrityTechnologies.business_manager.modules.supplier.model;

import com.IntegrityTechnologies.business_manager.modules.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.user.model.User;
import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Entity
@Table(name = "suppliers",
        indexes = { @Index(name = "idx_supplier_name", columnList = "name"),
                @Index(name = "idx_supplier_region", columnList = "region") })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Supplier {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false, unique = true)
    private String name;

    @ElementCollection
    @CollectionTable(name = "supplier_emails", joinColumns = @JoinColumn(name = "supplier_id"))
    @Column(name = "email")
    @Builder.Default
    private List<String> email = new ArrayList<>();

    @ElementCollection
    @CollectionTable(name = "supplier_phone_numbers", joinColumns = @JoinColumn(name = "supplier_id"))
    @Column(name = "phone_number")
    @Builder.Default
    private List<String> phoneNumber = new ArrayList<>();

    private String address;
    private String region;
    private Double rating;

    @OneToMany(mappedBy = "supplier", cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @Builder.Default
    private List<SupplierImage> images = new ArrayList<>();

    @ManyToMany(fetch = FetchType.LAZY)
    @JoinTable(
            name = "supplier_categories",
            joinColumns = @JoinColumn(name = "supplier_id"),
            inverseJoinColumns = @JoinColumn(name = "category_id")
    )
    @Builder.Default
    private Set<Category> categories = new HashSet<>();

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "created_by_id")
    private User createdBy;

    private LocalDateTime createdAt;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "updated_by_id")
    private User updatedBy;
    private LocalDateTime updatedAt;
    private LocalDateTime deletedAt;
    private boolean deleted = false;

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