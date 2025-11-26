package com.IntegrityTechnologies.business_manager.modules.department.model;

import com.IntegrityTechnologies.business_manager.modules.user.model.User;
import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalTime;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

@Entity
@Table(name = "departments")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Department {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(nullable = false, unique = true)
    private String name;

    // Optional description
    private String description;

    /**
     * Department may have multiple heads (SUPERVISOR role expected).
     */
    @ToString.Exclude
    @JsonIgnore
    @ManyToMany(fetch = FetchType.LAZY)
    @JoinTable(
            name = "department_members",
            joinColumns = @JoinColumn(name = "department_id"),
            inverseJoinColumns = @JoinColumn(name = "user_id")
    )
    @Builder.Default
    private Set<User> members = new HashSet<>();

    @ToString.Exclude
    @JsonIgnore
    @ManyToMany(fetch = FetchType.LAZY)
    @JoinTable(
            name = "department_heads",
            joinColumns = @JoinColumn(name = "department_id"),
            inverseJoinColumns = @JoinColumn(name = "user_id")
    )
    @Builder.Default
    private Set<User> heads = new HashSet<>();

        /**
     * Rollcall schedule: when rollcall window opens for the department.
     * Use LocalTime only (daily). You can extend to complex schedules.
     */
    private LocalTime rollcallStartTime;   // e.g. 09:00
    private Integer gracePeriodMinutes;    // e.g. 15 -> those after are LATE

    private boolean deleted = false;

    // --- SAFE HELPERS ---
    public void addMember(User user) {
        if (user == null) return;
        members.add(user);
    }

    public void removeMember(User user) {
        if (user == null) return;
        members.remove(user);
    }

    public void addHead(User user) {
        if (user == null) return;
        heads.add(user);
    }

    public void removeHead(User user) {
        if (user == null) return;
        heads.remove(user);
    }
}