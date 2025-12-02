package com.IntegrityTechnologies.business_manager.modules.finance.accounts.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounts.model.PartTransaction;

import java.math.BigDecimal;
import java.util.UUID;

public interface AccountingService {

    /**
     * Record a payment into the accounting ledger.
     * In DOUBLE_ENTRY mode -> create balanced journal entry.
     * In SINGLE_ENTRY mode -> update wallet account.
     */
    void recordPayment(UUID paymentId,
                       UUID saleId,
                       BigDecimal amount,
                       String method,
                       String reference,
                       String performedBy);

    /**
     * Record a PO receipt:
     * DOUBLE_ENTRY: Debit Inventory, Credit Payables
     * SINGLE_ENTRY: Log as PartTransaction
     */
    void recordPurchaseReceipt(UUID purchaseOrderId,
                               UUID supplierId,
                               BigDecimal amount,
                               String performedBy);

    /**
     * Convenience wrapper to record miscellaneous ledger part-transactions.
     */
    void recordPartTransaction(PartTransaction pt);
}