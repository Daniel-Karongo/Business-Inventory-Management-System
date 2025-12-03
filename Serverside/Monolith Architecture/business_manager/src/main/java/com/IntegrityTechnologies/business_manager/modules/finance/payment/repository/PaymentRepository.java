package com.IntegrityTechnologies.business_manager.modules.finance.payment.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.model.Payment;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Repository
public interface PaymentRepository extends JpaRepository<Payment, UUID> {
    List<Payment> findBySaleId(UUID saleId);
    List<Payment> findByProviderReference(String providerReference);
    List<Payment> findByTimestampBetween(LocalDateTime from, LocalDateTime to);
}