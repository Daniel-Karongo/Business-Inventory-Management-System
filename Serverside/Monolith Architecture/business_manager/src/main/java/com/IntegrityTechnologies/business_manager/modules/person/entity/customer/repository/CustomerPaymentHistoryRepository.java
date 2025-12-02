package com.IntegrityTechnologies.business_manager.modules.person.entity.customer.repository;

import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.model.CustomerPaymentHistory;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface CustomerPaymentHistoryRepository extends JpaRepository<CustomerPaymentHistory, UUID> {
    List<CustomerPaymentHistory> findByCustomerId(UUID customerId);
}