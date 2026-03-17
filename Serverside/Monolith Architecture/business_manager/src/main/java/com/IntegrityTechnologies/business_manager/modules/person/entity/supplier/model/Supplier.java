package com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.CategorySupplier;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.hibernate.annotations.UuidGenerator;

import java.time.LocalDateTime;
import java.util.*;

@Entity
@Table(
        name = "suppliers",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_supplier_tenant_branch_name",
                        columnNames = {"tenant_id", "branch_id", "name"}
                )
        },
        indexes = {
                @Index(name = "idx_supplier_tenant_branch", columnList = "tenant_id, branch_id"),
                @Index(name = "idx_supplier_name", columnList = "name")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@EqualsAndHashCode(callSuper = true, onlyExplicitlyIncluded = true)
@ToString(exclude = {"images", "email", "phoneNumber"})
public class Supplier extends BranchAwareEntity {

    @Id
    @GeneratedValue
    @UuidGenerator
    @EqualsAndHashCode.Include
    private UUID id;

    @Column(nullable = false)
    private String name;

    @ElementCollection
    @CollectionTable(
            name = "supplier_emails",
            joinColumns = @JoinColumn(name = "supplier_id")
    )
    @Column(name = "email")
    private Set<String> email = new HashSet<>();

    @ElementCollection
    @CollectionTable(
            name = "supplier_phone_numbers",
            joinColumns = @JoinColumn(name = "supplier_id")
    )
    @Column(name = "phone_number")
    private Set<String> phoneNumber = new HashSet<>();

    private String address;
    private String region;
    private Double rating;

    @OneToMany(
            mappedBy = "supplier",
            cascade = CascadeType.ALL,
            orphanRemoval = true,
            fetch = FetchType.LAZY
    )
    @Builder.Default
    private Set<SupplierImage> images = new HashSet<>();

    @Column(nullable = false)
    private String uploadFolder;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "created_by_id")
    private User createdBy;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "updated_by_id")
    private User updatedBy;

    private LocalDateTime deletedAt;

    @Column(nullable = false)
    private Boolean deleted;

    @OneToMany(
            mappedBy = "supplier",
            cascade = CascadeType.ALL,
            orphanRemoval = true
    )
    private Set<CategorySupplier> categorySuppliers = new HashSet<>();

    @PrePersist
    public void onCreate() {
        deleted = false;
    }
}