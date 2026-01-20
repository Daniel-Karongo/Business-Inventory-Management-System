package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.service;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.SmsAccount;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.repository.SmsAccountRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class SmsAccountService {

    private final SmsAccountRepository accountRepo;

    /**
     * Creates or updates the system SMS account.
     * Ensures ONLY ONE active account exists.
     */
    @Transactional
    public SmsAccount saveAndActivate(SmsAccount account) {

        // 🔒 Deactivate any existing active account
        accountRepo.findByActiveTrue().ifPresent(existing -> {
            existing.setActive(false);
            accountRepo.save(existing);
        });

        account.setActive(true);
        return accountRepo.save(account);
    }
}