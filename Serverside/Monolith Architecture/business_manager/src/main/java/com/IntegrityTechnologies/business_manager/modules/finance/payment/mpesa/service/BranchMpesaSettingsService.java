package com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.service;

import com.IntegrityTechnologies.business_manager.config.util.SecretMaskingUtil;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.dto.BranchMpesaSettingsDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.dto.BranchMpesaSettingsResponseDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.model.BranchMpesaSettings;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.repository.BranchMpesaSettingsRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.persistence.EntityNotFoundException;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class BranchMpesaSettingsService {

    private final BranchMpesaSettingsRepository repo;
    private final BranchTenantGuard branchTenantGuard;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    public BranchMpesaSettingsResponseDTO get(UUID branchId) {

        branchTenantGuard.validate(branchId);

        BranchMpesaSettings settings =
                repo
                        .findByTenantIdAndBranchIdAndDeletedFalse(
                                tenantId(),
                                branchId
                        )
                        .orElseThrow(() ->
                                new EntityNotFoundException(
                                        "M-Pesa settings not found"
                                )
                        );

        return BranchMpesaSettingsResponseDTO
                .builder()
                .enabled(settings.getEnabled())
                .sandbox(settings.getSandbox())
                .shortcode(settings.getShortcode())
                .consumerKey(
                        SecretMaskingUtil.mask(
                                settings.getConsumerKey()
                        )
                )
                .consumerSecret(
                        SecretMaskingUtil.mask(
                                settings.getConsumerSecret()
                        )
                )
                .passkey(
                        SecretMaskingUtil.mask(
                                settings.getPasskey()
                        )
                )
                .securityCredential(
                        SecretMaskingUtil.mask(
                                settings.getSecurityCredential()
                        )
                )
                .stkCallbackUrl(
                        settings.getStkCallbackUrl()
                )
                .c2bValidationUrl(
                        settings.getC2bValidationUrl()
                )
                .c2bConfirmationUrl(
                        settings.getC2bConfirmationUrl()
                )
                .initiatorName(
                        settings.getInitiatorName()
                )
                .active(settings.getActive())
                .build();
    }

    @Transactional
    public BranchMpesaSettings update(
            UUID branchId,
            BranchMpesaSettingsDTO dto
    ) {

        branchTenantGuard.validate(branchId);

        BranchMpesaSettings settings =
                repo.findByTenantIdAndBranchIdAndDeletedFalse(
                                tenantId(),
                                branchId
                        )
                        .orElseThrow(() ->
                                new EntityNotFoundException(
                                        "M-Pesa settings not found"
                                )
                        );

        settings.setEnabled(dto.getEnabled());
        settings.setSandbox(dto.getSandbox());
        settings.setShortcode(dto.getShortcode());

        if (dto.getConsumerKey() != null
                && !dto.getConsumerKey().isBlank()) {

            settings.setConsumerKey(dto.getConsumerKey());
        }

        if (dto.getConsumerSecret() != null
                && !dto.getConsumerSecret().isBlank()) {

            settings.setConsumerSecret(
                    dto.getConsumerSecret()
            );
        }

        if (dto.getPasskey() != null
                && !dto.getPasskey().isBlank()) {

            settings.setPasskey(dto.getPasskey());
        }

        if (dto.getSecurityCredential() != null
                && !dto.getSecurityCredential().isBlank()) {

            settings.setSecurityCredential(
                    dto.getSecurityCredential()
            );
        }

        settings.setStkCallbackUrl(
                dto.getStkCallbackUrl()
        );

        settings.setC2bValidationUrl(
                dto.getC2bValidationUrl()
        );

        settings.setC2bConfirmationUrl(
                dto.getC2bConfirmationUrl()
        );

        settings.setInitiatorName(
                dto.getInitiatorName()
        );

        settings.setActive(dto.getActive());

        return repo.save(settings);
    }
}