package com.IntegrityTechnologies.business_manager.modules.finance.accounting.projection;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "financial_projection_checkpoint",
        uniqueConstraints = @UniqueConstraint(
                columnNames = {"projectionName","branchId"}
        )
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ProjectionCheckpoint {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    private String projectionName;

    private UUID branchId;

    private LocalDateTime lastProcessedAt;
}