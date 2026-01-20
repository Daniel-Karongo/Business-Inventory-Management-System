package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.config;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.config.SmsProperties;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.SmsAccount;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.repository.SmsAccountRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Component
@RequiredArgsConstructor
public class SmsAccountInitializer implements ApplicationRunner {

    private final SmsAccountRepository accountRepo;
    private final SmsProperties props;

    @Override
    @Transactional
    public void run(ApplicationArguments args) {

        if (accountRepo.count() > 0) {
            log.info("SMS account already exists. Skipping initialization.");
            return;
        }

        SmsAccount account = SmsAccount.builder()
                .provider("AFRICAS_TALKING")
                .username(props.getAfrica().getUsername())
                .apiKey(props.getAfrica().getApiKey())
                .senderId(props.getAfrica().getSenderId())
                .active(true)
                .build();

        accountRepo.save(account);

        log.info("Default SMS account initialized using Africa's Talking [{} mode]",
                props.getEnv());
    }
}