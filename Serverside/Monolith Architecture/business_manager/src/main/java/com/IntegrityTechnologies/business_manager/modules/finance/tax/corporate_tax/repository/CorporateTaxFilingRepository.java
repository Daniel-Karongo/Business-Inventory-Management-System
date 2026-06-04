package com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.model.CorporateTaxFiling;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.model.CorporateTaxFilingStatus;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface CorporateTaxFilingRepository
        extends JpaRepository<CorporateTaxFiling, UUID> {

    boolean existsByTenantIdAndPeriod_IdAndBranchId(
            UUID tenantId,
            UUID periodId,
            UUID branchId
    );

    Optional<CorporateTaxFiling>
    findTopByTenantIdAndBranchIdOrderByFiledAtDesc(
            UUID tenantId,
            UUID branchId
    );

    Page<CorporateTaxFiling> findByTenantIdAndBranchId(
            UUID tenantId,
            UUID branchId,
            Pageable pageable
    );

    Optional<CorporateTaxFiling>
    findByTenantIdAndId(
            UUID tenantId,
            UUID id
    );

    @Query("""
                SELECT COALESCE(SUM(f.taxAmount),0)
                FROM CorporateTaxFiling f
                WHERE f.tenantId = :tenantId
                  AND f.branchId = :branchId
            """)
    BigDecimal totalAccruedTax(
            UUID tenantId,
            UUID branchId
    );

    @Query("""
                SELECT COALESCE(SUM(f.paidAmount),0)
                FROM CorporateTaxFiling f
                WHERE f.tenantId = :tenantId
                  AND f.branchId = :branchId
            """)
    BigDecimal totalPaidTax(
            UUID tenantId,
            UUID branchId
    );

    long countByTenantIdAndBranchIdAndStatus(
            UUID tenantId,
            UUID branchId,
            CorporateTaxFilingStatus status
    );

    List<CorporateTaxFiling> findByTenantIdAndBranchId(
            UUID tenantId,
            UUID branchId
    );

    @Query("""
                SELECT COALESCE(SUM(f.outstandingAmount),0)
                FROM CorporateTaxFiling f
                WHERE f.tenantId = :tenantId
                  AND f.branchId = :branchId
                  AND f.status IN (
                        com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.model.CorporateTaxFilingStatus.ACCRUED,
                        com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.model.CorporateTaxFilingStatus.PARTIALLY_PAID,
                        com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.model.CorporateTaxFilingStatus.OVERDUE
                  )
            """)
    BigDecimal totalOutstandingTax(
            UUID tenantId,
            UUID branchId
    );
}