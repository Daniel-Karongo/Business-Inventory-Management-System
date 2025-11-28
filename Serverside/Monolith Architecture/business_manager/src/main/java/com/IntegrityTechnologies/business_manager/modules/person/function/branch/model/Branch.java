package com.IntegrityTechnologies.business_manager.modules.person.function.branch.model;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.UuidGenerator;

import java.util.UUID;

@Entity
@Table(name = "branches")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@ToString
public class Branch {

    @Id
    @GeneratedValue
    @UuidGenerator
    @ToString.Exclude
    private UUID id;

    @Column(nullable = false, unique = true)
    private String branchCode;

    @Column(nullable = false)
    private String name;

    private String location;

    private String phone;

    private String email;

    private Boolean deleted = false;
}