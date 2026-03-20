package com.IntegrityTechnologies.business_manager.modules.person.entity.customer.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.util.*;

@Entity
@Table(
        name = "customers",
        indexes = {
                @Index(name = "idx_customer_tenant_branch", columnList = "tenant_id, branch_id")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class Customer extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    private String name;

    @ElementCollection
    @CollectionTable(name = "customer_phone_numbers", joinColumns = @JoinColumn(name = "customer_id"))
    private List<String> phoneNumbers = new ArrayList<>();

    @ElementCollection
    @CollectionTable(name = "customer_emails", joinColumns = @JoinColumn(name = "customer_id"))
    private List<String> emailAddresses = new ArrayList<>();

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "group_id")
    private CustomerGroup group;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private CustomerType type;

    @Enumerated(EnumType.STRING)
    private Gender gender;       // MALE | FEMALE | OTHER (nullable)

    @Column(length = 2000)
    private String address;

    private String notes;

    @Column(nullable = false)
    private Boolean deleted = false;
}