package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.provider;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.BranchSmsSettings;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.*;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

import java.util.List;
import java.util.Map;

@Slf4j
@Component
@RequiredArgsConstructor
public class BranchSmsProviderAdapter {

    private final RestTemplate restTemplate;

    public SmsProviderResponse send(
            BranchSmsSettings settings,
            String to,
            String message
    ) {

        HttpHeaders headers =
                new HttpHeaders();

        headers.setContentType(
                MediaType.APPLICATION_FORM_URLENCODED
        );

        headers.set(
                "apiKey",
                settings.getApiKey()
        );

        MultiValueMap<String, String> body =
                new LinkedMultiValueMap<>();

        body.add(
                "username",
                settings.getUsername()
        );

        body.add(
                "to",
                to
        );

        body.add(
                "message",
                message
        );

        if (settings.getSenderId() != null
                && !settings.getSenderId().isBlank()) {

            body.add(
                    "from",
                    settings.getSenderId()
            );
        }

        HttpEntity<MultiValueMap<String, String>> request =
                new HttpEntity<>(body, headers);

        String url =
                Boolean.TRUE.equals(settings.getSandbox())
                        ? "https://api.sandbox.africastalking.com/version1/messaging"
                        : "https://api.africastalking.com/version1/messaging";

        try {

            ResponseEntity<Map> response =
                    restTemplate.postForEntity(
                            url,
                            request,
                            Map.class
                    );

            Map<?, ?> responseBody =
                    response.getBody();

            if (responseBody == null) {

                return new SmsProviderResponse(
                        false,
                        null,
                        "Empty response"
                );
            }

            if (Boolean.TRUE.equals(settings.getSandbox())) {

                return new SmsProviderResponse(
                        true,
                        "sandbox-" + System.currentTimeMillis(),
                        null
                );
            }

            Object smsDataObj =
                    responseBody.get("SMSMessageData");

            if (!(smsDataObj instanceof Map<?, ?> smsData)) {

                return new SmsProviderResponse(
                        false,
                        null,
                        "Missing SMSMessageData"
                );
            }

            Object recipientsObj =
                    smsData.get("Recipients");

            if (!(recipientsObj instanceof List<?> recipients)
                    || recipients.isEmpty()) {

                return new SmsProviderResponse(
                        false,
                        null,
                        "Missing recipients"
                );
            }

            Object first =
                    recipients.get(0);

            if (!(first instanceof Map<?, ?> recipient)) {

                return new SmsProviderResponse(
                        false,
                        null,
                        "Invalid recipient payload"
                );
            }

            Object messageId =
                    recipient.get("messageId");

            return new SmsProviderResponse(
                    true,
                    messageId != null
                            ? messageId.toString()
                            : null,
                    null
            );

        } catch (Exception e) {

            return new SmsProviderResponse(
                    false,
                    null,
                    e.getMessage()
            );
        }
    }
}