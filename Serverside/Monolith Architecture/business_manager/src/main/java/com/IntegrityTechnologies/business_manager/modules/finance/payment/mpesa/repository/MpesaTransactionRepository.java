package com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.model.MpesaTransaction;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.UUID;

@Repository
public interface MpesaTransactionRepository extends JpaRepository<MpesaTransaction, UUID> {
    Optional<MpesaTransaction> findByCheckoutRequestId(String checkoutRequestId);
}