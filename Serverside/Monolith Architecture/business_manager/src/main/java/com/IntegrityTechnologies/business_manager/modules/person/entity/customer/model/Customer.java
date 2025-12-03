package com.IntegrityTechnologies.business_manager.modules.person.entity.customer.model;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Entity
@Table(name = "customers")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Customer {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(nullable = false)
    private String name;

    @ElementCollection
    @CollectionTable(
            name = "customer_phone_numbers",
            joinColumns = @JoinColumn(name = "customer_id"),
            indexes = {
                    @Index(name = "idx_customer_phone_number", columnList = "phone_number")
            }
    )
    @Column(name = "phone_number")
    @Builder.Default
    private List<String> phoneNumbers = new ArrayList<>();

    @ElementCollection
    @CollectionTable(
            name = "customer_email_addresses",
            joinColumns = @JoinColumn(name = "customer_id"),
            indexes = {
                    @Index(name = "idx_customer_email_address", columnList = "email_address")
            }
    )
    @Column(name = "email_address")
    @Builder.Default
    private List<String> emailAddresses = new ArrayList<>();

    @Column(length = 2000)
    private String address;

    private String notes;

    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    @PrePersist
    public void prePersist() {
        createdAt = LocalDateTime.now();
    }

    @PreUpdate
    public void preUpdate() {
        updatedAt = LocalDateTime.now();
    }
}