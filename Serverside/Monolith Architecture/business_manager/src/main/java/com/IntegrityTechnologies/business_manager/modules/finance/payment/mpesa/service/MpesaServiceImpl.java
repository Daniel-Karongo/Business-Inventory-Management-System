package com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.service;

import com.IntegrityTechnologies.business_manager.config.util.PhoneAndEmailNormalizer;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.dto.PaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.client.MpesaClient;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.model.BranchMpesaSettings;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.model.MpesaTransaction;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.repository.BranchMpesaSettingsRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.repository.MpesaTransactionRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.repository.PaymentRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.service.PaymentService;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.repository.SaleRepository;
import com.IntegrityTechnologies.business_manager.modules.person.customer.dto.CustomerRequest;
import com.IntegrityTechnologies.business_manager.modules.person.customer.model.CustomerType;
import com.IntegrityTechnologies.business_manager.modules.person.customer.service.CustomerService;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.persistence.EntityNotFoundException;
import org.springframework.transaction.annotation.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

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
    private final BranchMpesaSettingsRepository settingsRepo;
    private final PaymentService paymentService;
    private final PaymentRepository paymentRepository;
    private final CustomerService customerService;
    private final SaleRepository saleRepository;
    private final BranchTenantGuard branchTenantGuard;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    @Override
    @Transactional
    public MpesaTransaction initiateStk(
            UUID branchId,
            UUID saleId,
            String phone,
            BigDecimal amount
    ) {

        branchTenantGuard.validate(branchId);

        Sale sale =
                saleRepository.findByTenantIdAndBranchIdAndId(tenantId(), branchId, saleId)
                        .orElseThrow(() ->
                                new EntityNotFoundException(
                                        "Sale not found"
                                )
                        );

        validateSaleOwnership(
                sale,
                branchId
        );

        BranchMpesaSettings settings =
                settingsRepo
                        .findByTenantIdAndBranchIdAndActiveTrueAndDeletedFalse(
                                tenantId(),
                                branchId
                        )
                        .orElseThrow(() ->
                                new IllegalStateException(
                                        "M-Pesa not configured for branch"
                                )
                        );

        if (!Boolean.TRUE.equals(settings.getEnabled())) {
            throw new IllegalStateException(
                    "M-Pesa disabled for branch"
            );
        }

        String normalizedPhone =
                PhoneAndEmailNormalizer.normalizePhone(phone);

        if (normalizedPhone == null
                || !normalizedPhone.startsWith("+")) {

            throw new IllegalArgumentException(
                    "Invalid phone number format"
            );
        }

        String mpesaPhone =
                normalizedPhone.substring(1);

        Map resp =
                client.initiateStkPush(
                        settings,
                        mpesaPhone,
                        amount.toPlainString(),
                        saleId.toString(),
                        "Payment for sale"
                );

        MpesaTransaction tx =
                MpesaTransaction.builder()
                        .tenantId(tenantId())
                        .branchId(branchId)
                        .saleId(saleId)
                        .amount(amount)
                        .phoneNumber(normalizedPhone)
                        .status("PENDING")
                        .timestamp(LocalDateTime.now())
                        .rawResponse(
                                resp != null
                                        ? resp.toString()
                                        : null
                        )
                        .build();

        if (resp != null) {

            Object checkoutRequestId =
                    resp.get("CheckoutRequestID");

            tx.setCheckoutRequestId(
                    checkoutRequestId != null
                            ? checkoutRequestId.toString()
                            : null
            );

            tx.setMerchantRequestId(
                    String.valueOf(
                            resp.getOrDefault(
                                    "MerchantRequestID",
                                    ""
                            )
                    )
            );
        }

        return repo.save(tx);
    }

    @Override
    @Transactional
    public void handleStkCallback(
            UUID branchId,
            Map<String, Object> callback
    ) {

        branchTenantGuard.validate(branchId);

        if (callback == null || callback.isEmpty()) {
            return;
        }

        Object bodyObj =
                callback.get("Body");

        if (!(bodyObj instanceof Map<?, ?> body)) {
            return;
        }

        Object stkObj =
                body.get("stkCallback");

        if (!(stkObj instanceof Map<?, ?> stkCallback)) {
            return;
        }

        String checkoutRequestId =
                String.valueOf(
                        stkCallback.get("CheckoutRequestID")
                );

        int resultCode =
                Integer.parseInt(
                        String.valueOf(
                                stkCallback.get("ResultCode")
                        )
                );

        String resultDesc =
                String.valueOf(
                        stkCallback.get("ResultDesc")
                );

        Map callbackMetadata =
                (Map) stkCallback.get("CallbackMetadata");

        MpesaTransaction tx =
                repo.findByTenantIdAndBranchIdAndCheckoutRequestId(
                                tenantId(),
                                branchId,
                                checkoutRequestId
                        )
                        .orElseThrow(() ->
                                new IllegalStateException(
                                        "Unknown CheckoutRequestID"
                                )
                        );

        if (tx.getBranchId() == null
                || !branchId.equals(tx.getBranchId())) {

            throw new SecurityException(
                    "Callback branch mismatch"
            );
        }

        if ("SUCCESS".equalsIgnoreCase(tx.getStatus())) {
            return;
        }

        BigDecimal callbackAmount = null;

        if (resultCode == 0) {

            String receipt = null;

            BigDecimal amount = null;

            if (callbackMetadata != null
                    && callbackMetadata.get("Item") instanceof List<?> items) {

                for (Object obj : items) {

                    if (!(obj instanceof Map<?, ?> it)) {
                        continue;
                    }

                    String name =
                            String.valueOf(it.get("Name"));

                    Object val =
                            it.get("Value");

                    if ("MpesaReceiptNumber"
                            .equalsIgnoreCase(name)) {

                        receipt = String.valueOf(val);

                    } else if ("Amount"
                            .equalsIgnoreCase(name)) {

                        amount =
                                new BigDecimal(
                                        String.valueOf(val)
                                );

                        callbackAmount = amount;
                    }
                }
            }

            validateCallbackIntegrity(
                    branchId,
                    tx,
                    callbackAmount
            );

            if (receipt != null) {

                boolean duplicateReceipt =
                        repo.findByTenantIdAndBranchIdAndMpesaReceiptNumber(
                                        tenantId(),
                                        branchId,
                                        receipt
                                )
                                .filter(existing ->
                                        !existing.getId().equals(tx.getId())
                                )
                                .isPresent();

                if (duplicateReceipt) {

                    throw new IllegalStateException(
                            "Duplicate M-Pesa receipt detected"
                    );
                }
            }

            tx.setMpesaReceiptNumber(receipt);
            tx.setAmount(amount);
            tx.setStatus("SUCCESS");

            tx.setRawResponse(
                    "SUCCESS | Receipt="
                            + receipt
                            + " | Amount="
                            + amount
            );

            tx.setTimestamp(LocalDateTime.now());

            repo.save(tx);

            Sale sale =
                    saleRepository.findByTenantIdAndBranchIdAndId(tenantId(), branchId, tx.getSaleId())
                            .orElseThrow(() ->
                                    new IllegalStateException(
                                            "Sale not found"
                                    )
                            );

            validateSaleOwnership(
                    sale,
                    branchId
            );

            if (sale.getCustomerId() == null) {

                CustomerRequest cr =
                        new CustomerRequest();

                cr.setPhone(tx.getPhoneNumber());

                cr.setName(
                        "Mpesa " + tx.getPhoneNumber()
                );

                cr.setType(
                        CustomerType.INDIVIDUAL
                );

                cr.setNotes(
                        "Auto-created from Mpesa STK payment"
                );

                UUID customerId =
                        customerService.findOrCreateCustomer(
                                branchId,
                                List.of(cr)
                        );

                sale.setCustomerId(customerId);

                saleRepository.save(sale);
            }

            boolean alreadyProcessed =
                    paymentRepository
                            .findByTenantIdAndBranchIdAndProviderReference(
                                    tenantId(),
                                    branchId,
                                    receipt
                            )
                            .isPresent();

            if (!alreadyProcessed) {

                PaymentRequest pr =
                        new PaymentRequest();

                pr.setSaleId(tx.getSaleId());
                pr.setAmount(amount);
                pr.setMethod("MPESA");
                pr.setProviderReference(receipt);
                pr.setNote("Mpesa STK");

                paymentService.processPayment(branchId, pr);
            }

        } else {

            tx.setStatus("FAILED");

            tx.setRawResponse(
                    "FAILED | Code="
                            + resultCode
                            + " | Desc="
                            + resultDesc
            );
        }

        tx.setTimestamp(LocalDateTime.now());

        repo.save(tx);
    }

    @Override
    @Transactional
    public void handleC2BConfirm(
            UUID branchId,
            Map<String, Object> payload
    ) {

        branchTenantGuard.validate(branchId);

        try {

            String receipt =
                    String.valueOf(
                            payload.getOrDefault(
                                    "TransID",
                                    payload.get("TransactionReceipt")
                            )
                    );

            BigDecimal amount =
                    new BigDecimal(
                            String.valueOf(
                                    payload.getOrDefault(
                                            "Amount",
                                            "0"
                                    )
                            )
                    );

            String accountRef =
                    String.valueOf(
                            payload.getOrDefault(
                                    "AccountReference",
                                    ""
                            )
                    );

            UUID saleId =
                    UUID.fromString(accountRef);

            boolean alreadyProcessed =
                    paymentRepository
                            .findByTenantIdAndBranchIdAndProviderReference(
                                    tenantId(),
                                    branchId,
                                    receipt
                            )
                            .isPresent();

            if (alreadyProcessed) {
                return;
            }

            PaymentRequest pr =
                    new PaymentRequest();

            pr.setSaleId(saleId);
            pr.setAmount(amount);
            pr.setMethod("MPESA");
            pr.setProviderReference(receipt);
            pr.setNote("C2B confirm");

            paymentService.processPayment(branchId, pr);

        } catch (Exception ignored) {
        }
    }

    @Override
    @Transactional
    public void handleC2BValidate(
            UUID branchId,
            Map<String, Object> payload
    ) {

        branchTenantGuard.validate(branchId);
    }

    private void validateSaleOwnership(
            Sale sale,
            UUID branchId
    ) {

        boolean belongs =
                sale.getLineItems()
                        .stream()
                        .allMatch(li ->
                                branchId.equals(
                                        li.getBranchId()
                                )
                        );

        if (!belongs) {

            throw new SecurityException(
                    "Sale does not belong to branch"
            );
        }
    }

    private void validateCallbackIntegrity(
            UUID branchId,
            MpesaTransaction tx,
            BigDecimal callbackAmount
    ) {

        if (tx == null) {
            throw new SecurityException(
                    "Transaction missing"
            );
        }

        if (tx.getBranchId() == null
                || !branchId.equals(tx.getBranchId())) {

            throw new SecurityException(
                    "Branch mismatch"
            );
        }

        Sale sale =
                saleRepository
                        .findByTenantIdAndBranchIdAndId(
                                tenantId(),
                                branchId,
                                tx.getSaleId()
                        )
                        .orElseThrow(() ->
                                new SecurityException(
                                        "Sale not found"
                                )
                        );

        if (!branchId.equals(sale.getBranchId())) {

            throw new SecurityException(
                    "Sale branch mismatch"
            );
        }

        if (callbackAmount == null) {

            throw new SecurityException(
                    "Callback amount missing"
            );
        }

        if (tx.getAmount() == null) {

            throw new SecurityException(
                    "Original transaction amount missing"
            );
        }

        if (callbackAmount.compareTo(tx.getAmount()) != 0) {

            throw new SecurityException(
                    "Callback amount mismatch"
            );
        }
    }
}