package com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity;

import jakarta.persistence.*;
import lombok.*;

import java.util.UUID;

@Entity
@Table(name = "subscription_plans")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Plan {

    @Id
    @GeneratedValue
    private UUID id;

    @Column(nullable = false, unique = true)
    private String code;

    private String name;

    private int maxUsers;

    private int maxBranches;

    private boolean inventoryEnabled;

    private boolean accountingEnabled;

    private boolean reportingEnabled;
    private int requestsPerMinute;

}