package com.IntegrityTechnologies.business_manager.modules.finance.accounts.service.impl;

import com.IntegrityTechnologies.business_manager.config.OptimisticRetryRunner;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.model.PartTransaction;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.repository.AccountRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.service.AccountingService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.service.PartTransactionService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.model.Account;
import lombok.RequiredArgsConstructor;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@RequiredArgsConstructor
public class SingleEntryAccountingService implements AccountingService {

    private final AccountRepository accountRepository;
    private final PartTransactionService partTransactionService;

    // wallet account code - ensure you have an account with this code or change accordingly
    private static final String WALLET_CODE = "WALLET";

    @Override
    public void recordPayment(UUID paymentId, UUID saleId, BigDecimal amount, String method, String reference, String performedBy, String transactionCode) {
        OptimisticRetryRunner.runWithRetry(() -> {
            Account wallet = accountRepository.findByCode("WALLET")
                    .orElseThrow(() -> new IllegalStateException("Wallet account missing"));

            BigDecimal newBalance = wallet.getBalance().add(amount);
            wallet.setBalance(newBalance);
            accountRepository.save(wallet);

            PartTransaction pt = new PartTransaction();
            pt.setRelatedModule("PAYMENT");
            pt.setReferenceId(paymentId);
            pt.setTransactionTotal(amount);
            pt.setCreatedBy(performedBy);
            pt.setTransactionDate(LocalDateTime.now());
            pt.setTransactionCode(transactionCode);

            partTransactionService.record(pt);
            return null;
        });
    }

    @Override
    @Transactional
    public void recordPurchaseReceipt(UUID purchaseOrderId, UUID supplierId, BigDecimal amount, String performedBy) {
        // Single-entry: decrement wallet if paid immediately or record part transaction
        PartTransaction pt = new PartTransaction();
        pt.setRelatedModule("PURCHASE_RECEIPT");
        pt.setReferenceId(purchaseOrderId);
        pt.setTransactionTotal(amount);
        pt.setTransactionDate(java.time.LocalDateTime.now());
        pt.setCreatedBy(performedBy);
        pt.setNote("Single-entry purchase receipt");
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

            Account wallet = accountRepository.findByCode("WALLET")
                    .orElseThrow(() -> new IllegalStateException("Wallet account missing"));

            // Decrease wallet balance
            wallet.setBalance(wallet.getBalance().subtract(amount));
            accountRepository.save(wallet);

            PartTransaction pt = new PartTransaction();
            pt.setRelatedModule("REFUND");
            pt.setReferenceId(paymentId);
            pt.setTransactionTotal(amount.negate());
            pt.setTransactionDate(LocalDateTime.now());
            pt.setCreatedBy(performedBy);
            pt.setTransactionCode("RFND-" + paymentId.toString().substring(0, 6));

            partTransactionService.record(pt);

            return null;
        });
    }

    @Override
    public void reverseSalePayment(UUID saleId, BigDecimal amount, String reference) {

        OptimisticRetryRunner.runWithRetry(() -> {

            Account wallet = accountRepository.findByCode("WALLET")
                    .orElseThrow(() -> new IllegalStateException("Wallet account missing"));

            wallet.setBalance(wallet.getBalance().subtract(amount));
            accountRepository.save(wallet);

            PartTransaction pt = new PartTransaction();
            pt.setRelatedModule("SALE_REVERSAL");
            pt.setReferenceId(saleId);
            pt.setTransactionTotal(amount.negate());
            pt.setTransactionDate(LocalDateTime.now());
            pt.setCreatedBy("SYSTEM");
            pt.setTransactionCode("SALE-RFND-" + saleId.toString().substring(0, 6));

            partTransactionService.record(pt);

            return null;
        });
    }

}