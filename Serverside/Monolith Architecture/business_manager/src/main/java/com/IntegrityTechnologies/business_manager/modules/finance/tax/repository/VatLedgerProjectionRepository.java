package com.IntegrityTechnologies.business_manager.modules.finance.tax.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.VatLedgerProjection;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface VatLedgerProjectionRepository
        extends JpaRepository<VatLedgerProjection, UUID> {

    Optional<VatLedgerProjection> findByBranchIdAndFiscalYearAndMonthNumber(
            UUID branchId,
            int fiscalYear,
            int monthNumber
    );
}