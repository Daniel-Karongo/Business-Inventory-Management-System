package com.IntegrityTechnologies.business_manager.modules.supplier.repository;

import com.IntegrityTechnologies.business_manager.modules.supplier.model.SupplierAudit;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface SupplierAuditRepository extends JpaRepository<SupplierAudit, Long> {
    List<SupplierAudit> findBySupplierIdOrderByTimestampDesc(Long supplierId);
}