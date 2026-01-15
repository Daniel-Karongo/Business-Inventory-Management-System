package com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.service;

import com.IntegrityTechnologies.business_manager.common.PhoneAndEmailNormalizer;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.client.MpesaClient;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.config.MpesaProperties;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.model.MpesaTransaction;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.repository.MpesaTransactionRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.repository.PaymentRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.service.PaymentService;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.repository.SaleRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.dto.CustomerRequest;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.model.CustomerType;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.service.CustomerService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
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
    private final CustomerService customerService;
    private final SaleRepository saleRepository;

    @Override
    @Transactional
    public MpesaTransaction initiateStk(UUID saleId, String phone, BigDecimal amount) {

        String normalizedPhone =
                PhoneAndEmailNormalizer.normalizePhone(phone);

        if (normalizedPhone == null || !normalizedPhone.startsWith("+")) {
            throw new IllegalArgumentException("Invalid phone number format");
        }

        // Mpesa requires NO '+'
        String mpesaPhone = normalizedPhone.substring(1);

        Map resp = client.initiateStkPush(
                mpesaPhone,
                amount.toPlainString(),
                saleId != null ? saleId.toString() : "SALE",
                "Payment for sale",
                props.getStk().getCallbackUrl()
        );

        MpesaTransaction tx = MpesaTransaction.builder()
                .saleId(saleId)
                .amount(amount)
                .phoneNumber(normalizedPhone)   // ✅ STORE +254…
                .status("PENDING")
                .timestamp(LocalDateTime.now())
                .rawResponse(resp != null ? resp.toString() : null)
                .build();

        if (resp != null) {
            Object checkoutRequestId = resp.get("CheckoutRequestID");
            tx.setCheckoutRequestId(
                    checkoutRequestId != null ? checkoutRequestId.toString() : null
            );
            tx.setMerchantRequestId(
                    resp.getOrDefault("MerchantRequestID", "").toString()
            );
        }

        return repo.save(tx);
    }

    @Override
    @Transactional
    public void handleStkCallback(Map<String, Object> callback) {

        if (callback == null || callback.isEmpty()) {
            return;
        }

        Object bodyObj = callback.get("Body");
        if (!(bodyObj instanceof Map<?, ?> body)) {
            return;
        }

        Object stkObj = body.get("stkCallback");
        if (!(stkObj instanceof Map<?, ?> stkCallback)) {
            return;
        }

        String checkoutRequestId =
                String.valueOf(stkCallback.get("CheckoutRequestID"));

        int resultCode =
                Integer.parseInt(String.valueOf(stkCallback.get("ResultCode")));

        String resultDesc =
                String.valueOf(stkCallback.get("ResultDesc"));

        Map callbackMetadata =
                (Map) stkCallback.get("CallbackMetadata");

        MpesaTransaction tx =
                repo.findByCheckoutRequestId(checkoutRequestId)
                        .orElseThrow(() ->
                                new IllegalStateException(
                                        "Unknown CheckoutRequestID: " + checkoutRequestId
                                )
                        );

        if (resultCode == 0) {

            String receipt = null;
            BigDecimal amount = null;

            if (callbackMetadata != null && callbackMetadata.get("Item") instanceof List<?> items) {
                for (Object obj : items) {
                    if (!(obj instanceof Map<?, ?> it)) continue;

                    String name = String.valueOf(it.get("Name"));
                    Object val = it.get("Value");

                    if ("MpesaReceiptNumber".equalsIgnoreCase(name)) {
                        receipt = String.valueOf(val);
                    } else if ("Amount".equalsIgnoreCase(name)) {
                        amount = new BigDecimal(String.valueOf(val));
                    }
                }
            }

            tx.setMpesaReceiptNumber(receipt);
            tx.setAmount(amount);
            tx.setStatus("SUCCESS");
            tx.setRawResponse("SUCCESS | Receipt=" + receipt + " | Amount=" + amount);
            tx.setTimestamp(LocalDateTime.now());

            repo.save(tx);

            // 🔑 NEW LOGIC: ensure customer exists
            Sale sale = saleRepository.findById(tx.getSaleId())
                    .orElseThrow(() -> new IllegalStateException("Sale not found"));

            if (sale.getCustomerId() == null) {

                String phone = tx.getPhoneNumber();

                CustomerRequest cr = new CustomerRequest();
                cr.setPhone(phone);
                cr.setName("Mpesa " + phone);   // ← traceable + unique
                cr.setType(CustomerType.INDIVIDUAL);
                cr.setNotes("Auto-created from Mpesa STK payment");

                UUID customerId =
                        customerService.findOrCreateCustomer(List.of(cr));

                sale.setCustomerId(customerId);
                saleRepository.save(sale);
            }

            // 🔁 idempotent payment creation (UNCHANGED)
            var existing = paymentRepository.findByProviderReference(receipt);

            if (existing == null || existing.isEmpty()) {
                PaymentRequest pr = new PaymentRequest();
                pr.setSaleId(tx.getSaleId());
                pr.setAmount(amount);
                pr.setMethod("MPESA");
                pr.setProviderReference(receipt);
                pr.setNote("Mpesa STK");

                paymentService.processPayment(pr);
            }
        } else {
            tx.setStatus("FAILED");
            tx.setRawResponse(
                    "FAILED | Code=" + resultCode + " | Desc=" + resultDesc
            );
        }

        tx.setTimestamp(LocalDateTime.now());
        repo.save(tx);
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
}