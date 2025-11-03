package com.IntegrityTechnologies.business_manager.modules.supplier.repository;

import com.IntegrityTechnologies.business_manager.modules.supplier.model.SupplierImageAudit;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface SupplierImageAuditRepository extends JpaRepository<SupplierImageAudit, Long> {
    List<SupplierImageAudit> findBySupplierId(Long supplierId);
}