package com.IntegrityTechnologies.business_manager.modules.person.function.auth.service;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.dto.SmsRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.service.SmsService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class PasswordResetSmsService {

    private final SmsService smsService;

    @Value("${app.frontend.reset-password-url:http://localhost:4200/auth/reset-password}")
    private String resetUrl;

    public void sendResetSms(User user, String rawToken) {

        if (user.getPhoneNumbers() == null || user.getPhoneNumbers().isEmpty()) {
            return;
        }

        String link = resetUrl + "?token=" + rawToken;

        SmsRequest req = new SmsRequest();
        req.setToPhone(user.getPhoneNumbers().get(0));
        req.setMessage(
                "Password reset requested. Use this link: " + link
        );
        req.setCreatedBy("SYSTEM");

        smsService.sendSms(req);
    }
}