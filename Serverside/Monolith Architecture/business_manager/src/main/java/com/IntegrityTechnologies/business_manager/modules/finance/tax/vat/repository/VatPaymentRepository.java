package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.VatPayment;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Repository
public interface VatPaymentRepository
        extends JpaRepository<VatPayment, UUID> {

    List<VatPayment> findByTenantIdAndFiling_Id(
            UUID tenantId,
            UUID filingId
    );

    @Query("""
                select coalesce(sum(v.amount),0)
                from VatPayment v
                where v.tenantId=:tenantId
                and v.filing.id=:filingId
            """)
    BigDecimal totalPaidForFiling(
            UUID tenantId,
            UUID filingId
    );

    @Query("""
                select coalesce(sum(v.amount),0)
                from VatPayment v
                where v.tenantId=:tenantId
                and v.branchId=:branchId
            """)
    BigDecimal totalPaid(
            UUID tenantId,
            UUID branchId
    );

    @Query("""
            select coalesce(sum(v.amount),0)
            from VatPayment v
            where v.tenantId=:tenantId
            and v.branchId=:branchId
            """)
    BigDecimal totalPaidByBranch(
            UUID tenantId,
            UUID branchId
    );
}