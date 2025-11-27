package com.IntegrityTechnologies.business_manager.modules.communication.purchaseorder.repository;

import com.IntegrityTechnologies.business_manager.modules.communication.purchaseorder.model.PurchaseOrder;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface PurchaseOrderRepository extends JpaRepository<PurchaseOrder, UUID> {
}