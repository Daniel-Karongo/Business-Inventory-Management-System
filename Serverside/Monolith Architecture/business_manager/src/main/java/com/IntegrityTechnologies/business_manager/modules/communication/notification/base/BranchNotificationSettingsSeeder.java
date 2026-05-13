package com.IntegrityTechnologies.business_manager.modules.communication.notification.base;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.model.BranchEmailSettings;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.repository.BranchEmailSettingsRepository;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.BranchSmsSettings;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.repository.BranchSmsSettingsRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.model.BranchMpesaSettings;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.repository.BranchMpesaSettingsRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class BranchNotificationSettingsSeeder {

    private final BranchNotificationSettingsRepository notificationRepo;
    private final BranchSmsSettingsRepository smsRepo;
    private final BranchEmailSettingsRepository emailRepo;
    private final BranchMpesaSettingsRepository mpesaRepo;

    public void seed(
            UUID tenantId,
            UUID branchId
    ) {

        if (notificationRepo
                .findByTenantIdAndBranchIdAndDeletedFalse(
                        tenantId,
                        branchId
                )
                .isEmpty()) {

            notificationRepo.save(
                    BranchNotificationSettings.builder()
                            .tenantId(tenantId)
                            .branchId(branchId)
                            .smsEnabled(true)
                            .emailEnabled(true)
                            .allowRetries(true)
                            .maxRetryCount(3)
                            .build()
            );
        }

        if (smsRepo
                .findByTenantIdAndBranchIdAndDeletedFalse(
                        tenantId,
                        branchId
                )
                .isEmpty()) {

            smsRepo.save(
                    BranchSmsSettings.builder()
                            .tenantId(tenantId)
                            .branchId(branchId)
                            .enabled(false)
                            .provider("AFRICAS_TALKING")
                            .senderId("BUSINESS")
                            .defaultCountryCode("+254")
                            .sandbox(true)
                            .active(true)
                            .username("")
                            .apiKey("")
                            .build()
            );
        }

        if (emailRepo
                .findByTenantIdAndBranchIdAndDeletedFalse(
                        tenantId,
                        branchId
                )
                .isEmpty()) {

            emailRepo.save(
                    BranchEmailSettings.builder()
                            .tenantId(tenantId)
                            .branchId(branchId)
                            .enabled(false)
                            .host("")
                            .port(587)
                            .username("")
                            .password("")
                            .fromAddress("")
                            .tlsEnabled(true)
                            .authEnabled(true)
                            .active(true)
                            .build()
            );
        }

        if (mpesaRepo
                .findByTenantIdAndBranchIdAndDeletedFalse(
                        tenantId,
                        branchId
                )
                .isEmpty()) {

            mpesaRepo.save(
                    BranchMpesaSettings.builder()
                            .tenantId(tenantId)
                            .branchId(branchId)
                            .enabled(false)
                            .sandbox(true)
                            .consumerKey("")
                            .consumerSecret("")
                            .shortcode("")
                            .passkey("")
                            .stkCallbackUrl("")
                            .initiatorName("")
                            .securityCredential("")
                            .active(true)
                            .build()
            );
        }
    }
}