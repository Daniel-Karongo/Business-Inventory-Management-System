package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "sms_messages",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_sms_provider_message",
                        columnNames = {
                                "tenant_id",
                                "branch_id",
                                "provider_message_id"
                        }
                )
        },
        indexes = {

                @Index(
                        name = "idx_sms_tenant_branch",
                        columnList = "tenant_id, branch_id"
                ),

                @Index(
                        name = "idx_sms_status",
                        columnList = "status"
                ),

                @Index(
                        name = "idx_sms_phone",
                        columnList = "to_phone"
                ),

                @Index(
                        name = "idx_sms_provider_msg",
                        columnList = "provider_message_id"
                ),

                @Index(
                        name = "idx_sms_retry",
                        columnList = "tenant_id, branch_id, status, next_retry_at"
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
public class SmsMessage extends BranchAwareEntity {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(name = "to_phone", nullable = false)
    private String toPhone;

    private String fromName;

    @Column(nullable = false, length = 1000)
    private String message;

    @Column(nullable = false)
    private String status;

    @Column(name = "provider_message_id", length = 100)
    private String providerMessageId;

    private LocalDateTime sentAt;

    @Column(length = 1000)
    private String error;

    private String createdBy;

    private int retryCount;

    private LocalDateTime nextRetryAt;
}