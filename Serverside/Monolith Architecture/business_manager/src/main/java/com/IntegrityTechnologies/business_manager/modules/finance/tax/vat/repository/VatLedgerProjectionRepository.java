package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.VatLedgerProjection;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface VatLedgerProjectionRepository
        extends JpaRepository<VatLedgerProjection, UUID> {

    Optional<VatLedgerProjection>
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