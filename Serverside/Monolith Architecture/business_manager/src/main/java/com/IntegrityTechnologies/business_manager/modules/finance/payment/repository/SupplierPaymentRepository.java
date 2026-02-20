package com.IntegrityTechnologies.business_manager.modules.finance.payment.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.domain.SupplierPayment;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface SupplierPaymentRepository extends JpaRepository<SupplierPayment, UUID> {
}