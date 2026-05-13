package com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import com.IntegrityTechnologies.business_manager.security.crypto.EncryptedStringConverter;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "branch_mpesa_settings",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_branch_mpesa_settings",
                        columnNames = {
                                "tenant_id",
                                "branch_id"
                        }
                )
        },
        indexes = {
                @Index(
                        name = "idx_branch_mpesa_settings",
                        columnList = "tenant_id, branch_id"
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
public class BranchMpesaSettings extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false)
    private Boolean enabled = false;

    @Column(nullable = false)
    private Boolean sandbox = true;

    @Column(nullable = false)
    private String shortcode;

    @Lob
    @Convert(converter = EncryptedStringConverter.class)
    @Column(nullable = false, columnDefinition = "TEXT")
    private String consumerKey;

    @Lob
    @Convert(converter = EncryptedStringConverter.class)
    @Column(nullable = false, columnDefinition = "TEXT")
    private String consumerSecret;

    @Lob
    @Convert(converter = EncryptedStringConverter.class)
    @Column(nullable = false, columnDefinition = "TEXT")
    private String passkey;

    @Lob
    @Convert(converter = EncryptedStringConverter.class)
    @Column(nullable = false, columnDefinition = "TEXT")
    private String securityCredential;

    @Column(nullable = false)
    private String stkCallbackUrl;

    private String c2bValidationUrl;

    private String c2bConfirmationUrl;

    @Column(nullable = false)
    private String initiatorName;

    @Column(nullable = false)
    private Boolean active = true;

    @Column(nullable = false)
    @Builder.Default
    private Boolean credentialsMigrated = false;

    private LocalDateTime credentialsMigratedAt;
}