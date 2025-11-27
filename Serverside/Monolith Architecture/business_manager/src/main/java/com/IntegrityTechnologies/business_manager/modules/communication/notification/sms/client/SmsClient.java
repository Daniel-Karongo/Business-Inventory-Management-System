package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.client;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.config.SmsProperties;
import lombok.RequiredArgsConstructor;
import lombok.Synchronized;
import org.springframework.http.*;
import org.springframework.stereotype.Component;
import java.util.Base64;
import org.springframework.web.client.RestTemplate;

import java.time.Instant;
import java.util.HashMap;
import java.util.Map;

@Component
@RequiredArgsConstructor
public class SmsClient {

    private final SmsProperties props;
    private final RestTemplate restTemplate = new RestTemplate();

    // simple cached token
    private String cachedToken;
    private Instant tokenExpiry;

    private String baseUrl() {
        return "sandbox".equalsIgnoreCase(props.getEnv()) ? props.getBaseUrlSandbox() : props.getBaseUrlProduction();
    }

    @Synchronized
    private String getAccessToken() {
        if (cachedToken != null && tokenExpiry != null && Instant.now().isBefore(tokenExpiry.minusSeconds(30))) {
            return cachedToken;
        }

        // Daraja-style OAuth token endpoint: /oauth/v1/generate?grant_type=client_credentials
        String url = baseUrl() + "/oauth/v1/generate?grant_type=client_credentials";
        String auth = props.getConsumerKey() + ":" + props.getConsumerSecret();
        String basic = Base64.getEncoder().encodeToString(auth.getBytes());
        HttpHeaders headers = new HttpHeaders();
        headers.set("Authorization", "Basic " + basic);
        headers.setAccept(java.util.List.of(MediaType.APPLICATION_JSON));
        HttpEntity<Void> entity = new HttpEntity<>(headers);

        ResponseEntity<Map> resp = restTemplate.exchange(url, HttpMethod.GET, entity, Map.class);
        if (resp.getStatusCode().is2xxSuccessful() && resp.getBody() != null) {
            Map body = resp.getBody();
            Object token = body.get("access_token");
            Object expiresIn = body.get("expires_in");
            if (token != null) {
                cachedToken = token.toString();
                long seconds = (expiresIn != null) ? Long.parseLong(expiresIn.toString()) : 3600L;
                tokenExpiry = Instant.now().plusSeconds(seconds);
                return cachedToken;
            }
        }
        throw new RuntimeException("Failed to obtain SMS OAuth token");
    }

    public Map sendSms(String phoneE164, String message, String sender) {
        String token = getAccessToken();
        String url = baseUrl() + "/mpesa/sms/v1/send"; // daraja-like path for Customer SMS
        HttpHeaders headers = new HttpHeaders();
        headers.setBearerAuth(token);
        headers.setContentType(MediaType.APPLICATION_JSON);

        Map<String, Object> payload = new HashMap<>();
        payload.put("sender", sender != null ? sender : props.getSenderName());
        payload.put("receiver", phoneE164);
        payload.put("message", message);

        HttpEntity<Map<String, Object>> entity = new HttpEntity<>(payload, headers);
        ResponseEntity<Map> resp = restTemplate.postForEntity(url, entity, Map.class);
        return resp.getBody();
    }
}