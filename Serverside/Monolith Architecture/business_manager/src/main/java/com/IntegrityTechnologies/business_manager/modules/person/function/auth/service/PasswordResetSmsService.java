package com.IntegrityTechnologies.business_manager.modules.person.function.auth.service;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.dto.SmsRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.service.SmsService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class PasswordResetSmsService {

    private final SmsService smsService;

    public void sendResetSms(User user, String otp) {

        if (user.getPhoneNumbers() == null || user.getPhoneNumbers().isEmpty()) {
            return; // silent fail
        }

        SmsRequest req = new SmsRequest();
        req.setToPhone(user.getPhoneNumbers().get(0));
        req.setMessage(
                "Your password reset code is: " + otp +
                        ". This code expires in 10 minutes."
        );
        req.setCreatedBy("SYSTEM");

        smsService.sendSms(req);
    }
}