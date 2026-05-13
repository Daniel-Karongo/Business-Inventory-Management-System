package com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.client;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.model.BranchMpesaSettings;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.util.MpesaUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.http.*;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

import java.util.Base64;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Component
@RequiredArgsConstructor
public class MpesaClient {

    private final RestTemplate restTemplate;

    private final Map<String, String> tokens =
            new ConcurrentHashMap<>();

    private final Map<String, Long> expiries =
            new ConcurrentHashMap<>();

    private String baseUrl(
            BranchMpesaSettings settings
    ) {

        return Boolean.TRUE.equals(settings.getSandbox())
                ? "https://sandbox.safaricom.co.ke"
                : "https://api.safaricom.co.ke";
    }

    private synchronized String getAccessToken(
            BranchMpesaSettings settings
    ) {

        String key =
                tokenKey(settings);

        long now =
                System.currentTimeMillis() / 1000;

        String existing =
                tokens.get(key);

        Long expiry =
                expiries.get(key);

        if (existing != null
                && expiry != null
                && now < expiry - 30) {

            return existing;
        }

        String url =
                baseUrl(settings)
                        + "/oauth/v1/generate?grant_type=client_credentials";

        String auth =
                settings.getConsumerKey()
                        + ":"
                        + settings.getConsumerSecret();

        String basic =
                Base64.getEncoder()
                        .encodeToString(auth.getBytes());

        HttpHeaders headers =
                new HttpHeaders();

        headers.set(
                "Authorization",
                "Basic " + basic
        );

        headers.setAccept(
                List.of(MediaType.APPLICATION_JSON)
        );

        HttpEntity<Void> entity =
                new HttpEntity<>(headers);

        ResponseEntity<Map> resp =
                restTemplate.exchange(
                        url,
                        HttpMethod.GET,
                        entity,
                        Map.class
                );

        if (resp.getStatusCode().is2xxSuccessful()
                && resp.getBody() != null) {

            Map body =
                    resp.getBody();

            Object tk =
                    body.get("access_token");

            Object expires =
                    body.get("expires_in");

            if (tk != null) {

                String token =
                        tk.toString();

                long expiryEpoch =
                        now + (
                                expires != null
                                        ? Long.parseLong(
                                        expires.toString()
                                )
                                        : 3600L
                        );

                tokens.put(key, token);
                expiries.put(key, expiryEpoch);

                return token;
            }
        }

        throw new RuntimeException(
                "Failed to get Mpesa access token"
        );
    }

    private String tokenKey(
            BranchMpesaSettings settings
    ) {

        return settings.getTenantId()
                + ":"
                + settings.getBranchId()
                + ":"
                + settings.getConsumerKey();
    }

    public Map initiateStkPush(
            BranchMpesaSettings settings,
            String phone,
            String amount,
            String accountReference,
            String transactionDesc
    ) {

        String url =
                baseUrl(settings)
                        + "/mpesa/stkpush/v1/processrequest";

        String timestamp =
                MpesaUtil.timestamp();

        String password =
                MpesaUtil.generateStkPassword(
                        settings,
                        timestamp
                );

        Map<String, Object> payload =
                new HashMap<>();

        payload.put(
                "BusinessShortCode",
                settings.getShortcode()
        );

        payload.put(
                "Password",
                password
        );

        payload.put(
                "Timestamp",
                timestamp
        );

        payload.put(
                "TransactionType",
                "CustomerPayBillOnline"
        );

        payload.put(
                "Amount",
                Integer.parseInt(amount)
        );

        payload.put(
                "PartyA",
                phone
        );

        payload.put(
                "PartyB",
                settings.getShortcode()
        );

        payload.put(
                "PhoneNumber",
                phone
        );

        payload.put(
                "CallBackURL",
                settings.getStkCallbackUrl()
        );

        payload.put(
                "AccountReference",
                accountReference
        );

        payload.put(
                "TransactionDesc",
                transactionDesc
        );

        HttpHeaders headers =
                new HttpHeaders();

        headers.setBearerAuth(
                getAccessToken(settings)
        );

        headers.setContentType(
                MediaType.APPLICATION_JSON
        );

        HttpEntity<Map<String, Object>> entity =
                new HttpEntity<>(payload, headers);

        ResponseEntity<Map> resp =
                restTemplate.postForEntity(
                        url,
                        entity,
                        Map.class
                );

        return resp.getBody();
    }
}