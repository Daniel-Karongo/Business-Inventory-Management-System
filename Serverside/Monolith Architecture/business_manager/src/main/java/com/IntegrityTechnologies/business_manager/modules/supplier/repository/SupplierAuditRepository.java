package com.IntegrityTechnologies.business_manager.modules.supplier.repository;

import com.IntegrityTechnologies.business_manager.modules.supplier.model.SupplierAudit;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface SupplierAuditRepository extends JpaRepository<SupplierAudit, UUID> {
    List<SupplierAudit> findBySupplierIdOrderByTimestampDesc(UUID supplierId);
    List<SupplierAudit> findAllByOrderByTimestampDesc();
}