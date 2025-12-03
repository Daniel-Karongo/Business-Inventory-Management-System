package com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.service;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.client.MpesaClient;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.config.MpesaProperties;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.model.MpesaTransaction;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.repository.MpesaTransactionRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.repository.PaymentRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.service.PaymentService;
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
    private final PaymentService paymentService;
    private final PaymentRepository paymentRepository;

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
            tx.setCheckoutRequestId(checkoutRequestId != null ? checkoutRequestId.toString() : null);
            tx.setMerchantRequestId(resp.getOrDefault("MerchantRequestID", "").toString());
        }
        return repo.save(tx);
    }

    @Override
    @Transactional
    public void handleStkCallback(Map<String, Object> callback) {
        // your existing logic with idempotent payment creation (keeps behavior)
        try {
            Map body = (Map) callback.get("Body");
            Map stkCallback = (Map) body.get("stkCallback");
            String checkoutRequestId = String.valueOf(stkCallback.get("CheckoutRequestID"));
            int resultCode = Integer.parseInt(String.valueOf(stkCallback.get("ResultCode")));
            Map callbackMetadata = (Map) stkCallback.get("CallbackMetadata");

            MpesaTransaction tx = repo.findByCheckoutRequestId(checkoutRequestId).orElse(null);
            if (tx == null) {
                tx = MpesaTransaction.builder().id(UUID.randomUUID()).checkoutRequestId(checkoutRequestId).build();
            }

            if (resultCode == 0) {
                String receipt = "";
                String amountStr = null;
                if (callbackMetadata != null && callbackMetadata.get("Item") instanceof java.util.List) {
                    java.util.List items = (java.util.List) callbackMetadata.get("Item");
                    for (Object obj : items) {
                        if (!(obj instanceof Map)) continue;
                        Map it = (Map) obj;
                        String name = String.valueOf(it.get("Name"));
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

            if ("SUCCESS".equalsIgnoreCase(tx.getStatus()) && tx.getSaleId() != null && tx.getMpesaReceiptNumber() != null) {
                var existing = paymentRepository.findByProviderReference(tx.getMpesaReceiptNumber());
                if (existing == null || existing.isEmpty()) {
                    PaymentRequest pr = new PaymentRequest();
                    pr.setSaleId(tx.getSaleId());
                    pr.setAmount(tx.getAmount() == null ? BigDecimal.ZERO : tx.getAmount());
                    pr.setMethod("MPESA");
                    pr.setProviderReference(tx.getMpesaReceiptNumber());
                    pr.setNote("Mpesa STK callback");
                    try {
                        paymentService.processPayment(pr);
                    } catch (Exception ex) {
                        System.err.println("Failed to create payment from mpesa callback: " + ex.getMessage());
                    }
                }
            }
        } catch (Exception ex) {
            throw new RuntimeException("Failed to process STK callback: " + ex.getMessage(), ex);
        }
    }

    @Override
    @Transactional
    public void handleC2BConfirm(Map payload) {
        // Safaricom C2B confirm: parse payload and create payment where appropriate
        try {
            // parsing according to expected c2b payload keys
            String msisdn = String.valueOf(payload.getOrDefault("MSISDN", payload.getOrDefault("msisdn", "")));
            String receipt = String.valueOf(payload.getOrDefault("TransID", payload.getOrDefault("TransID", payload.get("TransactionReceipt"))));
            BigDecimal amount = new BigDecimal(String.valueOf(payload.getOrDefault("Amount", "0")));
            String accountRef = String.valueOf(payload.getOrDefault("AccountReference", payload.getOrDefault("accountReference", "")));
            // attempt to map accountRef to saleId (if you encode saleId in accountRef)
            UUID saleId = null;
            try {
                saleId = UUID.fromString(accountRef);
            } catch (Exception ignored) {}

            // idempotent create payment (if receipt not already present)
            var existing = paymentRepository.findByProviderReference(receipt);
            if (existing == null || existing.isEmpty()) {
                PaymentRequest pr = new PaymentRequest();
                pr.setSaleId(saleId);
                pr.setAmount(amount);
                pr.setMethod("MPESA");
                pr.setProviderReference(receipt);
                pr.setNote("C2B confirm");
                paymentService.processPayment(pr);
            }
        } catch (Exception ex) {
            System.err.println("Failed to process C2B confirm: " + ex.getMessage());
        }
    }

    @Override
    @Transactional
    public void handleC2BValidate(Map payload) {
        // basic accept/validate; you can implement business rules here
        System.out.println("C2B validation payload: " + payload);
    }

    private String normalizePhone(String p) {
        if (p == null) return null;
        String phone = p.trim();
        if (phone.startsWith("0")) return "254" + phone.substring(1);
        if (phone.startsWith("+")) return phone.substring(1);
        return phone;
    }
}