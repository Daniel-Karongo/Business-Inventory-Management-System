package com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.projection.CorporateTaxLedgerProjection;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface CorporateTaxLedgerProjectionRepository
        extends JpaRepository<CorporateTaxLedgerProjection, UUID> {

    Optional<CorporateTaxLedgerProjection>
    findByTenantIdAndBranchIdAndFiscalYearAndMonthNumber(
            UUID tenantId,
            UUID branchId,
            int fiscalYear,
            int monthNumber
    );

    void deleteByTenantIdAndBranchId(
            UUID tenantId,
            UUID branchId
    );
}