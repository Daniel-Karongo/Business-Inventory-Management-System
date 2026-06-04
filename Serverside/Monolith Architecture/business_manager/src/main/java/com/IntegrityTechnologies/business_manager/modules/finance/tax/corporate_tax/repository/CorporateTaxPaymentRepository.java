package com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.model.CorporateTaxPayment;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

public interface CorporateTaxPaymentRepository
        extends JpaRepository<CorporateTaxPayment, UUID> {

    List<CorporateTaxPayment>
    findByTenantIdAndBranchIdAndFiling_IdOrderByRecordedAtAsc(
            UUID tenantId,
            UUID branchId,
            UUID filingId
    );

    @Query("""
                SELECT COALESCE(SUM(p.amount),0)
                FROM CorporateTaxPayment p
                WHERE p.tenantId = :tenantId
                  AND p.branchId = :branchId
            """)
    BigDecimal totalPaid(
            UUID tenantId,
            UUID branchId
    );
}