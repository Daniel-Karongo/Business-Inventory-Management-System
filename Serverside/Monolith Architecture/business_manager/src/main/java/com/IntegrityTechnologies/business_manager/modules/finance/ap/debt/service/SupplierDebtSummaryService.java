package com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.service;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.dto.SupplierDebtSummaryDto;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.projection.SupplierDebtSummaryProjection;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.repository.SupplierDebtSummaryRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class SupplierDebtSummaryService {

    private final SupplierDebtSummaryRepository repository;

    public Page<SupplierDebtSummaryDto> getDebtSummary(
            UUID branchId,
            String search,
            Boolean hasOverdue,
            Boolean hasUnapplied,
            Pageable pageable
    ) {
        return repository.findDebtSummary(
                TenantContext.getTenantId(),
                branchId,
                LocalDate.now(),
                normalize(search),
                hasOverdue,
                hasUnapplied,
                pageable
        ).map(this::map);
    }

    private String normalize(String value) {

        if (value == null || value.isBlank()) {
            return null;
        }

        return value.trim();
    }

    private SupplierDebtSummaryDto map(
            SupplierDebtSummaryProjection projection
    ) {
        return SupplierDebtSummaryDto.builder()
                .supplierId(projection.getSupplierId())
                .supplierName(projection.getSupplierName())
                .totalOutstanding(zero(projection.getTotalOutstanding()))
                .hasOverdue(projection.getOverdueBills() > 0)
                .hasUnappliedPayments(zero(projection.getUnappliedPayments()).compareTo(BigDecimal.ZERO) > 0)
                .overdueAmount(zero(projection.getOverdueAmount()))
                .unappliedPayments(zero(projection.getUnappliedPayments()))
                .netPayable(zero(projection.getNetPayable()))
                .openBills(projection.getOpenBills())
                .overdueBills(projection.getOverdueBills())
                .oldestDueDate(projection.getOldestDueDate())
                .lastPaymentDate(projection.getLastPaymentDate())
                .riskLevel(riskLevel(
                        zero(projection.getOverdueAmount()),
                        zero(projection.getTotalOutstanding())
                ))
                .build();
    }

    private BigDecimal zero(BigDecimal value) {
        return value == null ? BigDecimal.ZERO : value;
    }

    private String riskLevel(
            BigDecimal overdue,
            BigDecimal outstanding
    ) {
        if (outstanding.compareTo(BigDecimal.ZERO) <= 0) {
            return "CLEAR";
        }

        if (overdue.compareTo(BigDecimal.ZERO) <= 0) {
            return "GOOD";
        }

        BigDecimal ratio = overdue.divide(
                outstanding,
                4,
                java.math.RoundingMode.HALF_UP
        );

        if (ratio.compareTo(new BigDecimal("0.50")) >= 0) {
            return "HIGH";
        }

        if (ratio.compareTo(new BigDecimal("0.20")) >= 0) {
            return "MEDIUM";
        }

        return "LOW";
    }
}