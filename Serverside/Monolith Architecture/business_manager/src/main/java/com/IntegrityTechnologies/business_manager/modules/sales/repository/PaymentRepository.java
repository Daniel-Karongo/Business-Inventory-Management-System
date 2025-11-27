package com.IntegrityTechnologies.business_manager.modules.sales.repository;

import com.IntegrityTechnologies.business_manager.modules.sales.model.PaymentRecord;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface PaymentRepository extends JpaRepository<PaymentRecord, UUID> {
}