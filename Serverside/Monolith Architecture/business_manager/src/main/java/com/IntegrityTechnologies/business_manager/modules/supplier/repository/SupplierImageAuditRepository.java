package com.IntegrityTechnologies.business_manager.modules.supplier.repository;

import com.IntegrityTechnologies.business_manager.modules.supplier.model.SupplierImageAudit;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface SupplierImageAuditRepository extends JpaRepository<SupplierImageAudit, UUID> {
    List<SupplierImageAudit> findBySupplierId(UUID supplierId);
}