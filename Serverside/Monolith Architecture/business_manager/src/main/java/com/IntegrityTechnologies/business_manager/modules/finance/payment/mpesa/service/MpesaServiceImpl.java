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
    private final PaymentService paymentService; // add bean
    private final PaymentRepository paymentRepository; // to check duplicates

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
                // success
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
                if (amountStr != null) tx.setAmount(new java.math.BigDecimal(amountStr));
                tx.setTimestamp(LocalDateTime.now());
            } else {
                tx.setStatus("FAILED");
            }
            tx.setRawResponse(callback.toString());
            repo.save(tx);

            // If success and linked to a sale -> create payment (idempotent: check providerReference)
            if ("SUCCESS".equalsIgnoreCase(tx.getStatus()) && tx.getSaleId() != null && tx.getMpesaReceiptNumber() != null) {
                // check existing payment with same providerReference
                var existing = paymentRepository.findByProviderReference(tx.getMpesaReceiptNumber());
                if (existing != null && !existing.isEmpty()) {
                    return; // already processed
                }

                PaymentRequest pr = new PaymentRequest();
                pr.setSaleId(tx.getSaleId());
                pr.setAmount(tx.getAmount() == null ? java.math.BigDecimal.ZERO : tx.getAmount());
                pr.setMethod("MPESA");
                pr.setProviderReference(tx.getMpesaReceiptNumber());
                pr.setNote("Mpesa STK callback");

                try {
                    paymentService.processPayment(pr);
                } catch (Exception ex) {
                    // log but do not fail the whole callback
                    System.err.println("Failed to create payment from mpesa callback: " + ex.getMessage());
                }
            }

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