package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.VatCreditMovement;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.UUID;

public interface VatCreditMovementRepository
        extends JpaRepository<VatCreditMovement, UUID> {

    List<VatCreditMovement> findByTenantIdAndBranchIdOrderByCreatedAtDesc(
            UUID tenantId,
            UUID branchId
    );
}