package com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.service;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.client.MpesaClient;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.config.MpesaProperties;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.model.MpesaTransaction;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.repository.MpesaTransactionRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Map;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class MpesaServiceImpl implements MpesaService {

    private final MpesaClient client;
    private final MpesaTransactionRepository repo;
    private final MpesaProperties props;

    @Override
    @Transactional
    public MpesaTransaction initiateStk(UUID saleId, String phone, BigDecimal amount) {
        String normalized = normalizePhone(phone);
        Map resp = client.initiateStkPush(normalized, amount.toPlainString(), saleId != null ? saleId.toString() : "SALE", "Payment for sale", props.getStk().getCallbackUrl());
        MpesaTransaction tx = MpesaTransaction.builder()
                .id(UUID.randomUUID())
                .saleId(saleId)
                .amount(amount)
                .phoneNumber(normalized)
                .status("PENDING")
                .timestamp(LocalDateTime.now())
                .rawResponse(resp != null ? resp.toString() : null)
                .build();

        if (resp != null) {
            Object checkoutRequestId = resp.get("CheckoutRequestID");
            if (checkoutRequestId == null) checkoutRequestId = resp.get("CheckoutRequestID"); // sometimes different case
            tx.setCheckoutRequestId(checkoutRequestId != null ? checkoutRequestId.toString() : null);
            tx.setMerchantRequestId(resp.getOrDefault("MerchantRequestID", "").toString());
        }
        return repo.save(tx);
    }

    @Override
    @Transactional
    public void handleStkCallback(Map callback) {
        // callback structure per Daraja: check ResultCode and Receipt
        // This method should be wired to the controller that receives JSON payload from Safaricom
        try {
            Map body = (Map) callback.get("Body");
            Map stkCallback = (Map) body.get("stkCallback");
            String checkoutRequestId = String.valueOf(stkCallback.get("CheckoutRequestID"));
            int resultCode = Integer.parseInt(String.valueOf(stkCallback.get("ResultCode")));
            Map metadata = (Map) ((Map) stkCallback.get("CallbackMetadata"));

            MpesaTransaction tx = repo.findByCheckoutRequestId(checkoutRequestId).orElse(null);
            if (tx == null) {
                tx = MpesaTransaction.builder().id(UUID.randomUUID()).checkoutRequestId(checkoutRequestId).build();
            }

            if (resultCode == 0) {
                // success
                // extract MpesaReceiptNumber and Amount from metadata
                String receipt = "";
                String amountStr = null;
                if (metadata != null && metadata.get("Item") instanceof java.util.List) {
                    java.util.List items = (java.util.List) metadata.get("Item");
                    for (Object obj : items) {
                        if (!(obj instanceof Map)) continue;
                        Map it = (Map) obj;
                        String name = (String) it.get("Name");
                        Object val = it.get("Value");
                        if ("MpesaReceiptNumber".equalsIgnoreCase(name) || "MpesaReceipt".equalsIgnoreCase(name)) {
                            receipt = String.valueOf(val);
                        } else if ("Amount".equalsIgnoreCase(name)) {
                            amountStr = String.valueOf(val);
                        }
                    }
                }
                tx.setMpesaReceiptNumber(receipt);
                tx.setStatus("SUCCESS");
                if (amountStr != null) tx.setAmount(new BigDecimal(amountStr));
                tx.setTimestamp(LocalDateTime.now());
            } else {
                tx.setStatus("FAILED");
            }
            tx.setRawResponse(callback.toString());
            repo.save(tx);
        } catch (Exception ex) {
            throw new RuntimeException("Failed to process STK callback: " + ex.getMessage(), ex);
        }
    }

    private String normalizePhone(String p) {
        if (p == null) return null;
        String phone = p.trim();
        if (phone.startsWith("0")) {
            return "254" + phone.substring(1);
        }
        if (phone.startsWith("+")) {
            return phone.substring(1);
        }
        return phone;
    }
}