package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.provider;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.config.SmsProperties;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.SmsAccount;
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
public class AfricasTalkingSmsProvider implements SmsProvider {

    private final SmsProperties props;
    private final RestTemplate restTemplate = new RestTemplate();

    private String baseUrl() {
        return "production".equalsIgnoreCase(props.getEnv())
                ? props.getAfrica().getBaseUrlProduction()
                : props.getAfrica().getBaseUrlSandbox();
    }

    @Override
    public SmsProviderResponse send(String to, String message, SmsAccount account) {

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
        headers.set("apiKey", account.getApiKey());

        MultiValueMap<String, String> body = new LinkedMultiValueMap<>();
        body.add("username", account.getUsername());
        body.add("to", to);
        body.add("message", message);

        // Sender ID is optional in sandbox but MUST NOT be null
        if (account.getSenderId() != null && !account.getSenderId().isBlank()) {
            body.add("from", account.getSenderId());
        }

        HttpEntity<MultiValueMap<String, String>> request =
                new HttpEntity<>(body, headers);

        try {
            ResponseEntity<Map> response =
                    restTemplate.postForEntity(baseUrl(), request, Map.class);

            // 🔍 LOG EVERYTHING FROM AFRICA'S TALKING
            log.info("Africa's Talking HTTP status: {}", response.getStatusCode());
            log.info("Africa's Talking raw response body: {}", response.getBody());


            Map<?, ?> responseBody = response.getBody();
            if (responseBody == null) {
                return new SmsProviderResponse(false, null, "Empty response body");
            }

            Object smsDataObj = responseBody.get("SMSMessageData");
            if (!(smsDataObj instanceof Map)) {
                return new SmsProviderResponse(false, null, "Missing SMSMessageData");
            }

            Map<?, ?> smsData = (Map<?, ?>) smsDataObj;
            Object recipientsObj = smsData.get("Recipients");

        /* =========================================================
           SANDBOX MODE — Africa's Talking returns inconsistent data
           ========================================================= */
            if ("sandbox".equalsIgnoreCase(props.getEnv())) {
                String syntheticId = "sandbox-" + System.currentTimeMillis();
                return new SmsProviderResponse(true, syntheticId, null);
            }

        /* =========================================================
           PRODUCTION MODE — strict parsing
           ========================================================= */
            if (!(recipientsObj instanceof List<?> recipients) || recipients.isEmpty()) {
                return new SmsProviderResponse(
                        false,
                        null,
                        "Invalid recipients response: " + recipientsObj
                );
            }

            Object firstObj = recipients.get(0);
            if (!(firstObj instanceof Map<?, ?> first)) {
                return new SmsProviderResponse(false, null, "Invalid recipient format");
            }

            Object messageId = first.get("messageId");
            if (messageId == null) {
                return new SmsProviderResponse(false, null, "Missing messageId");
            }

            return new SmsProviderResponse(true, messageId.toString(), null);

        } catch (Exception e) {
            return new SmsProviderResponse(false, null, e.getMessage());
        }
    }
}