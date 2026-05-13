package com.IntegrityTechnologies.business_manager.modules.communication.notification.migration;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.model.BranchEmailSettings;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.repository.BranchEmailSettingsRepository;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.BranchSmsSettings;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.repository.BranchSmsSettingsRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.model.BranchMpesaSettings;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.repository.BranchMpesaSettingsRepository;
import com.IntegrityTechnologies.business_manager.modules.person.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.Tenant;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.repository.TenantRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.UUID;

@Slf4j
@Component
@RequiredArgsConstructor
public class LegacyCredentialMigrationRunner implements ApplicationRunner {

    private final TenantRepository tenantRepository;
    private final BranchRepository branchRepository;

    private final BranchMpesaSettingsRepository mpesaRepo;
    private final BranchSmsSettingsRepository smsRepo;
    private final BranchEmailSettingsRepository emailRepo;

    @Value("${migration.legacy-credentials.enabled:true}")
    private boolean enabled;

    /* =========================================================
       LEGACY MPESA
       ========================================================= */

    @Value("${legacy.mpesa.consumer-key:}")
    private String mpesaConsumerKey;

    @Value("${legacy.mpesa.consumer-secret:}")
    private String mpesaConsumerSecret;

    @Value("${legacy.mpesa.shortcode:}")
    private String mpesaShortcode;

    @Value("${legacy.mpesa.passkey:}")
    private String mpesaPasskey;

    @Value("${legacy.mpesa.callback-url:}")
    private String mpesaCallbackUrl;

    @Value("${legacy.mpesa.payment-type:PAYBILL}")
    private String mpesaPaymentType;

    @Value("${legacy.mpesa.sandbox:true}")
    private Boolean mpesaSandbox;

    /* =========================================================
       LEGACY SMS
       ========================================================= */

    @Value("${legacy.sms.username:}")
    private String smsUsername;

    @Value("${legacy.sms.api-key:}")
    private String smsApiKey;

    @Value("${legacy.sms.sender-id:}")
    private String smsSenderId;

    @Value("${legacy.sms.default-country-code:+254}")
    private String smsCountryCode;

    @Value("${legacy.sms.sandbox:true}")
    private Boolean smsSandbox;

    /* =========================================================
       LEGACY EMAIL
       ========================================================= */

    @Value("${legacy.email.host:}")
    private String emailHost;

    @Value("${legacy.email.port:587}")
    private Integer emailPort;

    @Value("${legacy.email.username:}")
    private String emailUsername;

    @Value("${legacy.email.password:}")
    private String emailPassword;

    @Value("${legacy.email.from:}")
    private String emailFrom;

    @Override
    @Transactional
    public void run(ApplicationArguments args) {

        if (!enabled) {
            return;
        }

        log.warn(
                "Legacy credential migration runner is ENABLED. " +
                        "Disable after successful migration."
        );

        Tenant tenant =
                tenantRepository
                        .findByCodeIgnoreCase("default")
                        .orElse(null);

        if (tenant == null) {
            return;
        }

        Branch branch =
                branchRepository
                        .findByTenantIdAndBranchCodeIgnoreCaseAndDeletedFalse(
                                tenant.getId(),
                                "MAIN"
                        )
                        .orElse(null);

        if (branch == null) {
            return;
        }

        UUID tenantId = tenant.getId();
        UUID branchId = branch.getId();

        migrateMpesa(
                tenantId,
                branchId
        );

        migrateSms(
                tenantId,
                branchId
        );

        migrateEmail(
                tenantId,
                branchId
        );

        log.info(
                "Legacy credential migration completed for tenant={} branch={}",
                tenant.getCode(),
                branch.getBranchCode()
        );
    }

    private void migrateMpesa(
            UUID tenantId,
            UUID branchId
    ) {

        if (mpesaConsumerKey == null
                || mpesaConsumerKey.isBlank()) {

            return;
        }

        BranchMpesaSettings settings =
                mpesaRepo
                        .findByTenantIdAndBranchIdAndDeletedFalse(
                                tenantId,
                                branchId
                        )
                        .orElseThrow();

        if (Boolean.TRUE.equals(
                settings.getCredentialsMigrated()
        )) {
            return;
        }

        if (isPopulated(settings.getConsumerKey())) {

            settings.setCredentialsMigrated(true);
            settings.setCredentialsMigratedAt(
                    LocalDateTime.now()
            );

            mpesaRepo.save(settings);

            return;
        }

        settings.setEnabled(true);
        settings.setSandbox(mpesaSandbox);
        settings.setShortcode(mpesaShortcode);
        settings.setConsumerKey(mpesaConsumerKey);
        settings.setConsumerSecret(mpesaConsumerSecret);
        settings.setPasskey(mpesaPasskey);
        settings.setStkCallbackUrl(mpesaCallbackUrl);
        settings.setActive(true);

        settings.setCredentialsMigrated(true);

        settings.setCredentialsMigratedAt(
                LocalDateTime.now()
        );
        mpesaRepo.save(settings);

        log.info("Migrated legacy M-Pesa credentials");
    }

    private void migrateSms(
            UUID tenantId,
            UUID branchId
    ) {

        if (smsApiKey == null
                || smsApiKey.isBlank()) {

            return;
        }

        BranchSmsSettings settings =
                smsRepo
                        .findByTenantIdAndBranchIdAndDeletedFalse(
                                tenantId,
                                branchId
                        )
                        .orElseThrow();

        if (Boolean.TRUE.equals(
                settings.getCredentialsMigrated()
        )) {
            return;
        }

        if (isPopulated(settings.getApiKey())) {

            settings.setCredentialsMigrated(true);
            settings.setCredentialsMigratedAt(
                    LocalDateTime.now()
            );

            smsRepo.save(settings);

            return;
        }

        settings.setEnabled(true);
        settings.setProvider("AFRICAS_TALKING");
        settings.setUsername(smsUsername);
        settings.setApiKey(smsApiKey);
        settings.setSenderId(smsSenderId);
        settings.setSandbox(smsSandbox);
        settings.setDefaultCountryCode(smsCountryCode);
        settings.setActive(true);

        settings.setCredentialsMigrated(true);

        settings.setCredentialsMigratedAt(
                LocalDateTime.now()
        );
        smsRepo.save(settings);

        log.info("Migrated legacy SMS credentials");
    }

    private void migrateEmail(
            UUID tenantId,
            UUID branchId
    ) {

        if (emailPassword == null
                || emailPassword.isBlank()) {

            return;
        }

        BranchEmailSettings settings =
                emailRepo
                        .findByTenantIdAndBranchIdAndDeletedFalse(
                                tenantId,
                                branchId
                        )
                        .orElseThrow();

        if (Boolean.TRUE.equals(
                settings.getCredentialsMigrated()
        )) {
            return;
        }

        if (isPopulated(settings.getPassword())) {

            settings.setCredentialsMigrated(true);
            settings.setCredentialsMigratedAt(
                    LocalDateTime.now()
            );

            emailRepo.save(settings);

            return;
        }

        settings.setEnabled(true);
        settings.setHost(emailHost);
        settings.setPort(emailPort);
        settings.setUsername(emailUsername);
        settings.setPassword(emailPassword);
        settings.setFromAddress(emailFrom);
        settings.setAuthEnabled(true);
        settings.setTlsEnabled(true);
        settings.setActive(true);

        settings.setCredentialsMigrated(true);

        settings.setCredentialsMigratedAt(
                LocalDateTime.now()
        );
        emailRepo.save(settings);

        log.info("Migrated legacy email credentials");
    }

    private boolean isPopulated(String value) {
        return value != null && !value.isBlank();
    }
}