package com.IntegrityTechnologies.business_manager.modules.communication.notification.email.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.CollectionTable;
import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import jakarta.persistence.Index;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.Lob;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Entity
@Table(
        name = "email_messages",
        indexes = {
                @Index(
                        name = "idx_email_tenant_branch",
                        columnList = "tenant_id, branch_id"
                ),
                @Index(
                        name = "idx_email_status",
                        columnList = "status"
                ),
                @Index(
                        name = "idx_email_sent_at",
                        columnList = "sent_at"
                ),
                @Index(
                        name = "idx_email_retry",
                        columnList = "tenant_id, branch_id, status, next_retry_at"
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
public class EmailMessage extends BranchAwareEntity {

    @Id
    @GeneratedValue
    private UUID id;

    @ElementCollection
    @CollectionTable(
            name = "email_recipients",
            joinColumns = @JoinColumn(name = "email_message_id")
    )
    private List<String> recipients;

    private String subject;

    @Lob
    @Column(nullable = false)
    private String body;

    private String status;

    private String error;

    private LocalDateTime sentAt;

    private String createdBy;

    @Column(nullable = false)
    private int retryCount = 0;

    private LocalDateTime nextRetryAt;
}