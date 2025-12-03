package com.IntegrityTechnologies.business_manager.modules.finance.accounts.service.impl;

import com.IntegrityTechnologies.business_manager.config.OptimisticRetryRunner;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.dto.EntryLineRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.dto.JournalEntryRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.model.PartTransaction;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.repository.AccountRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.service.AccountingService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.service.JournalService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.service.PartTransactionService;
import jakarta.persistence.OptimisticLockException;
import lombok.RequiredArgsConstructor;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

@RequiredArgsConstructor
public class DoubleEntryAccountingService implements AccountingService {

    private final JournalService journalService;
    private final AccountRepository accountRepository;
    private final PartTransactionService partTransactionService;

    // Example codes - ensure seeded accounts exist with these codes or change to your codes
    private static final String CASH_CODE = "1000";      // cash/bank
    private static final String REVENUE_CODE = "4000";   // sales revenue
    private static final String INVENTORY_CODE = "1200"; // inventory

    @Override
    public void recordPayment(UUID paymentId, UUID saleId, BigDecimal amount,
                              String method, String reference, String performedBy,
                              String transactionCode) {

        OptimisticRetryRunner.runWithRetry(() -> {

            JournalEntryRequest req = new JournalEntryRequest();
            req.setReference("PAYMENT-" + paymentId);
            req.setTransactionCode(transactionCode);
            req.setDescription("Payment " + transactionCode + " for sale " + saleId);

            // Debit (Cash or Mpesa)
            EntryLineRequest debit = new EntryLineRequest();
            debit.setAccountId(resolveAccountIdForMethod(method));
            debit.setDebit(amount);
            debit.setTransactionType("PAYMENT");
            debit.setTransactionCode(transactionCode);

            // Credit (Revenue)
            EntryLineRequest credit = new EntryLineRequest();
            credit.setAccountId(
                    accountRepository.findByCode("4000")
                            .orElseThrow(() -> new IllegalStateException("Revenue account MISSING"))
                            .getId()
            );
            credit.setCredit(amount);
            credit.setTransactionType("PAYMENT");
            credit.setTransactionCode(transactionCode);

            req.setLines(Arrays.asList(debit, credit));

            journalService.createJournalEntry(req, performedBy);

            // PartTransaction (only one)
            PartTransaction pt = new PartTransaction();
            pt.setRelatedModule("PAYMENT");
            pt.setReferenceId(paymentId);
            pt.setTransactionTotal(amount);
            pt.setTransactionDate(LocalDateTime.now());
            pt.setCreatedBy(performedBy);
            pt.setTransactionCode(transactionCode);

            partTransactionService.record(pt);

            return null;
        });
    }

    private UUID resolveAccountIdForMethod(String method) {
        // map payment method to account code
        String code = switch (method == null ? "CASH" : method.toUpperCase()) {
            case "MPESA" -> "1001"; // if you create such accounts
            case "CARD" -> "1002";
            default -> "1000";
        };
        return accountRepository.findByCode(code)
                .orElseGet(() -> accountRepository.findByCode("1000").orElseThrow(() -> new IllegalStateException("Cash account missing")))
                .getId();
    }

    @Override
    @Transactional
    public void recordPurchaseReceipt(UUID purchaseOrderId, UUID supplierId, BigDecimal amount, String performedBy) {
        // Simple example: Debit Inventory, Credit Payables (or cash if paid)
        JournalEntryRequest req = new JournalEntryRequest();
        req.setReference("PO-RECEIPT-" + purchaseOrderId);
        req.setDescription("Purchase Order Received: " + purchaseOrderId);

        List<EntryLineRequest> lines = new ArrayList<>();
        var invAccount = accountRepository.findByCode(INVENTORY_CODE)
                .orElseThrow(() -> new IllegalStateException("Inventory account (" + INVENTORY_CODE + ") not configured"));

        EntryLineRequest debit = new EntryLineRequest();
        debit.setAccountId(invAccount.getId());
        debit.setDebit(amount);
        debit.setTransactionType("PURCHASE");
        lines.add(debit);

        // Use a Payables account code - ensure seeded or change below
        var payablesAccount = accountRepository.findByCode("2000")
                .orElseThrow(() -> new IllegalStateException("Payables account (2000) not configured"));

        EntryLineRequest credit = new EntryLineRequest();
        credit.setAccountId(payablesAccount.getId());
        credit.setCredit(amount);
        credit.setTransactionType("PURCHASE");
        lines.add(credit);

        req.setLines(lines);
        journalService.createJournalEntry(req, performedBy);

        PartTransaction pt = new PartTransaction();
        pt.setRelatedModule("PURCHASE_RECEIPT");
        pt.setReferenceId(purchaseOrderId);
        pt.setTransactionTotal(amount);
        pt.setTransactionDate(java.time.LocalDateTime.now());
        pt.setCreatedBy(performedBy);
        pt.setNote("Purchase received");
        partTransactionService.record(pt);
    }

    @Override
    public void recordPartTransaction(PartTransaction pt) {
        partTransactionService.record(pt);
    }

    @Override
    public void recordRefund(UUID paymentId,
                             UUID saleId,
                             BigDecimal amount,
                             String method,
                             String performedBy) {

        OptimisticRetryRunner.runWithRetry(() -> {

            String transactionCode = "RFND-" + paymentId.toString().substring(0, 6);

            JournalEntryRequest req = new JournalEntryRequest();
            req.setReference("REFUND-" + paymentId);
            req.setTransactionCode(transactionCode);
            req.setDescription("Refund for payment " + paymentId);

            // Reverse debit → credit cash
            EntryLineRequest creditCash = new EntryLineRequest();
            creditCash.setAccountId(resolveAccountIdForMethod(method));
            creditCash.setCredit(amount);
            creditCash.setTransactionType("REFUND");
            creditCash.setTransactionCode(transactionCode);

            // Reverse credit → debit revenue
            EntryLineRequest debitRevenue = new EntryLineRequest();
            debitRevenue.setAccountId(
                    accountRepository.findByCode("4000")
                            .orElseThrow(() -> new IllegalStateException("Revenue account missing"))
                            .getId()
            );
            debitRevenue.setDebit(amount);
            debitRevenue.setTransactionType("REFUND");
            debitRevenue.setTransactionCode(transactionCode);

            req.setLines(Arrays.asList(creditCash, debitRevenue));
            journalService.createJournalEntry(req, performedBy);

            // PartTransaction
            PartTransaction pt = new PartTransaction();
            pt.setRelatedModule("REFUND");
            pt.setReferenceId(paymentId);
            pt.setTransactionTotal(amount.negate()); // refunds are negative
            pt.setTransactionDate(LocalDateTime.now());
            pt.setCreatedBy(performedBy);
            pt.setTransactionCode(transactionCode);

            partTransactionService.record(pt);

            return null;
        });
    }
}