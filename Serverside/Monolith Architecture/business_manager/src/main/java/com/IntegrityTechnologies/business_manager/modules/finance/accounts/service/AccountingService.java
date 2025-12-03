package com.IntegrityTechnologies.business_manager.modules.finance.accounts.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounts.model.PartTransaction;

import java.math.BigDecimal;
import java.util.UUID;

public interface AccountingService {

    void recordPayment(UUID paymentId,
                       UUID saleId,
                       BigDecimal amount,
                       String method,
                       String reference,
                       String performedBy,
                       String transactionCode);

    void recordRefund(UUID paymentId,
                      UUID saleId,
                      BigDecimal amount,
                      String method,
                      String performedBy);

    void recordPurchaseReceipt(UUID purchaseOrderId,
                               UUID supplierId,
                               BigDecimal amount,
                               String performedBy);

    void recordPartTransaction(PartTransaction pt);
}
