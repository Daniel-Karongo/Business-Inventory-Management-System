package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.VatRefund;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.enums.VatRefundStatus;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface VatRefundRepository
        extends JpaRepository<VatRefund, UUID> {

    Optional<VatRefund> findByTenantIdAndFiling_Id(
            UUID tenantId,
            UUID filingId
    );

    List<VatRefund> findByTenantIdAndBranchId(
            UUID tenantId,
            UUID branchId
    );

    long countByTenantIdAndBranchIdAndStatus(
            UUID tenantId,
            UUID branchId,
            VatRefundStatus status
    );

    @Query("""
            select coalesce(sum(v.amount),0)
            from VatRefund v
            where v.tenantId=:tenantId
            and v.branchId=:branchId
            and v.status='REQUESTED'
            """)
    BigDecimal pendingRefundAmount(
            UUID tenantId,
            UUID branchId
    );
}