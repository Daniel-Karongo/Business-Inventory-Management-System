package com.IntegrityTechnologies.business_manager.modules.finance.accounting.projection;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "financial_projection_checkpoint",
        uniqueConstraints = @UniqueConstraint(
                columnNames = {"tenant_id","branch_id","projectionName"}
        )
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ProjectionCheckpoint extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    private String projectionName;

    private LocalDateTime lastProcessedAt;
}