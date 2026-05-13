package com.IntegrityTechnologies.business_manager.modules.communication.notification.base;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.util.UUID;

@Entity
@Table(
        name = "branch_notification_settings",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_branch_notification_settings",
                        columnNames = {
                                "tenant_id",
                                "branch_id"
                        }
                )
        },
        indexes = {
                @Index(
                        name = "idx_branch_notification_settings",
                        columnList = "tenant_id, branch_id"
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
public class BranchNotificationSettings extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false)
    private Boolean smsEnabled = true;

    @Column(nullable = false)
    private Boolean emailEnabled = true;

    @Column(nullable = false)
    private Boolean allowRetries = true;

    @Column(nullable = false)
    private Integer maxRetryCount = 3;
}