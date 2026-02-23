package com.IntegrityTechnologies.business_manager.modules.finance.payment.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.domain.SupplierPayment;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.repository.SupplierPaymentRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class SupplierPaymentService {

    private final SupplierPaymentRepository paymentRepository;
    private final AccountingFacade accountingFacade;
    private final AccountingAccounts accounts;

    @Transactional
    public SupplierPayment pay(
            UUID supplierId,
            BigDecimal amount,
            String method,
            String reference
    ) {

        if (amount == null || amount.compareTo(BigDecimal.ZERO) <= 0) {
            throw new IllegalArgumentException("Amount must be greater than zero");
        }

        UUID creditAccount = resolvePaymentAccount(method);

        // -----------------------------------------
        // ACCOUNTING ENTRY
        // DR Accounts Payable
        // CR Cash/Bank/Mpesa
        // -----------------------------------------
        accountingFacade.post(
                AccountingEvent.builder()
                        .sourceModule("SUPPLIER_PAYMENT")
                        .sourceId(supplierId)
                        .reference(reference)
                        .description("Supplier payment via " + method)
                        .performedBy(currentUser())
                        .entries(List.of(
                                AccountingEvent.Entry.builder()
                                        .accountId(accounts.accountsPayable())
                                        .direction(EntryDirection.DEBIT)
                                        .amount(amount)
                                        .build(),
                                AccountingEvent.Entry.builder()
                                        .accountId(creditAccount)
                                        .direction(EntryDirection.CREDIT)
                                        .amount(amount)
                                        .build()
                        ))
                        .build()
        );

        SupplierPayment payment = SupplierPayment.builder()
                .supplierId(supplierId)
                .amount(amount)
                .method(method.toUpperCase())
                .reference(reference)
                .paidAt(LocalDateTime.now())
                .paidBy(currentUser())
                .build();

        return paymentRepository.save(payment);
    }

    private UUID resolvePaymentAccount(String method) {

        return switch (method.toUpperCase()) {
            case "CASH" -> accounts.cash();
            case "BANK" -> accounts.bank();
            case "MPESA" -> accounts.mpesa();
            default -> throw new IllegalArgumentException("Unsupported payment method");
        };
    }

    private String currentUser() {
        var auth = SecurityContextHolder.getContext().getAuthentication();
        return auth != null ? auth.getName() : "SYSTEM";
    }

    public Page<SupplierPayment> list(Pageable pageable) {
        return paymentRepository.findAll(pageable);
    }
}