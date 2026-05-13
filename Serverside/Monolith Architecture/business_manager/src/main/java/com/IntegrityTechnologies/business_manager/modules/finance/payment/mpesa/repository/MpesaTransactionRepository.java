package com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.repository;

import aj.org.objectweb.asm.commons.Remapper;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.model.MpesaTransaction;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface MpesaTransactionRepository
        extends JpaRepository<MpesaTransaction, UUID> {

    Optional<MpesaTransaction>
    findByTenantIdAndBranchIdAndCheckoutRequestId(
            UUID tenantId,
            UUID branchId,
            String checkoutRequestId
    );

    Optional<MpesaTransaction>
    findByTenantIdAndBranchIdAndMpesaReceiptNumber(
            UUID tenantId,
            UUID branchId,
            String receipt
    );

    Optional<MpesaTransaction> findByTenantIdAndBranchIdAndId(
            UUID tenantId,
            UUID branchId,
            UUID id
    );
}