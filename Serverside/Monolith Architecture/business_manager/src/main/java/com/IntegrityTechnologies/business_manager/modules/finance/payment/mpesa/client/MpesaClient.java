package com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.client;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.config.MpesaProperties;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.util.MpesaUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.http.*;
import org.springframework.stereotype.Component;
import java.util.Base64;
import org.springframework.web.client.RestTemplate;

import java.util.HashMap;
import java.util.Map;

@Component
@RequiredArgsConstructor
public class MpesaClient {

    private final MpesaProperties props;
    private final RestTemplate restTemplate = new RestTemplate();

    private String token;
    private long tokenExpiryEpoch;

    private String baseUrl() {
        return "sandbox".equalsIgnoreCase(props.getEnv()) ? "https://sandbox.safaricom.co.ke" : "https://api.safaricom.co.ke";
    }

    private synchronized String getAccessToken() {
        long now = System.currentTimeMillis() / 1000;
        if (token != null && now < tokenExpiryEpoch - 30) return token;

        String url = baseUrl() + "/oauth/v1/generate?grant_type=client_credentials";
        String auth = props.getConsumerKey() + ":" + props.getConsumerSecret();
        String basic = Base64.getEncoder().encodeToString(auth.getBytes());
        HttpHeaders headers = new HttpHeaders();
        headers.set("Authorization", "Basic " + basic);
        headers.setAccept(java.util.List.of(MediaType.APPLICATION_JSON));
        HttpEntity<Void> entity = new HttpEntity<>(headers);
        ResponseEntity<Map> resp = restTemplate.exchange(url, HttpMethod.GET, entity, Map.class);
        if (resp.getStatusCode().is2xxSuccessful() && resp.getBody() != null) {
            Map b = resp.getBody();
            Object tk = b.get("access_token");
            Object expires = b.get("expires_in");
            if (tk != null) {
                token = tk.toString();
                tokenExpiryEpoch = now + ((expires != null) ? Long.parseLong(expires.toString()) : 3600L);
                return token;
            }
        }
        throw new RuntimeException("Failed to get Mpesa access token");
    }

    /**
     * Initiate STK Push (Lipa na M-Pesa Online)
     */
    public Map initiateStkPush(String phone, String amount, String accountReference, String transactionDesc, String callbackUrl) {
        String url = baseUrl() + "/mpesa/stkpush/v1/processrequest";
        String t = MpesaUtil.timestamp();
        String password = MpesaUtil.generateStkPassword(props, t);

        Map<String, Object> payload = new HashMap<>();
        payload.put("BusinessShortCode", props.getShortcode());
        payload.put("Password", password);
        payload.put("Timestamp", t);
        payload.put("TransactionType", "CustomerPayBillOnline");
        payload.put("Amount", amount);
        payload.put("PartyA", phone); // customer phone in 2547... form
        payload.put("PartyB", props.getShortcode());
        payload.put("PhoneNumber", phone);
        payload.put("CallBackURL", callbackUrl != null ? callbackUrl : props.getStk().getCallbackUrl());
        payload.put("AccountReference", accountReference);
        payload.put("TransactionDesc", transactionDesc);

        HttpHeaders headers = new HttpHeaders();
        headers.setBearerAuth(getAccessToken());
        headers.setContentType(MediaType.APPLICATION_JSON);
        HttpEntity<Map<String, Object>> entity = new HttpEntity<>(payload, headers);

        ResponseEntity<Map> resp = restTemplate.postForEntity(url, entity, Map.class);
        return resp.getBody();
    }
}