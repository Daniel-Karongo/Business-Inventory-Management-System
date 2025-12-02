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
    public void recordPayment(UUID paymentId, UUID saleId, BigDecimal amount, String method, String reference, String performedBy) {

        OptimisticRetryRunner.runWithRetry(() -> {
            // Build journal entry request (example — adapt to your DTO)
            JournalEntryRequest req = new JournalEntryRequest();
            req.setReference("PAYMENT-" + paymentId);
            req.setDescription("Payment " + reference + " for sale " + saleId);

            // create debit line -> Cash or Wallet
            EntryLineRequest cashLine = new EntryLineRequest();
            cashLine.setAccountId(resolveAccountIdForMethod(method)); // implement helper
            cashLine.setDebit(amount);
            cashLine.setCredit(null);
            cashLine.setNote("Payment received, method=" + method);
            cashLine.setTransactionType("PAYMENT");

            // create credit line -> Revenue
            EntryLineRequest revenueLine = new EntryLineRequest();
            revenueLine.setAccountId(accountRepository.findByCode("4000").orElseThrow(() -> new IllegalStateException("Revenue account missing")).getId());
            revenueLine.setDebit(null);
            revenueLine.setCredit(amount);
            revenueLine.setNote("Sales revenue");
            revenueLine.setTransactionType("PAYMENT");

            req.setLines(Arrays.asList(cashLine, revenueLine));

            journalService.createJournalEntry(req, performedBy);

            // record a part transaction pointing to the payment (optional)
            PartTransaction pt = new PartTransaction();
            pt.setRelatedModule("PAYMENT");
            pt.setReferenceId(paymentId);
            pt.setTransactionTotal(amount);
            pt.setCreatedBy(performedBy);
            pt.setTransactionDate(LocalDateTime.now());

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
}