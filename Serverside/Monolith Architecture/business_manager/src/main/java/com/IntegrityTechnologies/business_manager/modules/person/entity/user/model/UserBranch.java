package com.IntegrityTechnologies.business_manager.modules.person.entity.user.model;

import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

@Entity
@Table(
        name = "user_branches",
        uniqueConstraints = {
                @UniqueConstraint(columnNames = {"user_id", "branch_id"})
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserBranch {

    @EmbeddedId
    private UserBranchId id = new UserBranchId();

    @ManyToOne(fetch = FetchType.LAZY)
    @MapsId("userId")
    @JoinColumn(name = "user_id")
    private User user;

    @ManyToOne(fetch = FetchType.LAZY)
    @MapsId("branchId")
    @JoinColumn(name = "branch_id")
    private Branch branch;

    @Column(nullable = false)
    private boolean primaryBranch = false;

    private LocalDateTime assignedAt;

    @PrePersist
    public void onCreate() {

        assignedAt = LocalDateTime.now();

        if (id == null) {
            id = new UserBranchId();
        }

        if (user != null) {
            id.setUserId(user.getId());
        }

        if (branch != null) {
            id.setBranchId(branch.getId());
        }
    }
}