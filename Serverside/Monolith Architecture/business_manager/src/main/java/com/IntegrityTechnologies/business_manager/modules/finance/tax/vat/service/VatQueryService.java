package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountBalance;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountBalanceRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountingPeriodRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.base.model.TaxSystemState;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.base.service.TaxSystemStateService;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto.VatDashboardResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto.VatOverviewResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.mapper.VatStatusMapper;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.VatFiling;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.enums.VatRefundStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.repository.VatFilingRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.repository.VatPaymentRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.repository.VatRefundRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class VatQueryService {

    private final VatFilingRepository filingRepository;
    private final AccountingPeriodRepository periodRepository;
    private final AccountBalanceRepository accountBalanceRepository;
    private final AccountingAccounts accounts;
    private final TaxSystemStateService taxSystemStateService;
    private final VatPaymentRepository paymentRepository;
    private final VatRefundRepository refundRepository;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    public BigDecimal currentCredit(
            UUID branchId
    ) {
        return balance(
                branchId,
                "VAT_CARRY_FORWARD"
        );
    }

    public VatDashboardResponse dashboard(
            UUID branchId
    ) {

        TaxSystemState state =
                taxSystemStateService.getOrCreate(
                        branchId
                );

        BigDecimal payable =
                balance(
                        branchId,
                        "VAT_PAYABLE"
                );

        BigDecimal credit =
                balance(
                        branchId,
                        "VAT_CARRY_FORWARD"
                );

        VatFiling latestFiling =
                filingRepository
                        .findTopByTenantIdAndBranchIdOrderByFiledAtDesc(
                                tenantId(),
                                branchId
                        )
                        .orElse(null);

        AccountingPeriod nextUnfiled =
                periodRepository
                        .findByTenantIdAndBranchIdOrderByStartDateAsc(
                                tenantId(),
                                branchId
                        )
                        .stream()
                        .filter(
                                AccountingPeriod::isClosed
                        )
                        .filter(
                                p ->
                                        !filingRepository.existsByTenantIdAndPeriod_IdAndBranchId(
                                                tenantId(),
                                                p.getId(),
                                                branchId
                                        )
                        )
                        .findFirst()
                        .orElse(null);

        BigDecimal totalPaid =
                paymentRepository
                        .totalPaidByBranch(
                                tenantId(),
                                branchId
                        );

        BigDecimal pendingRefundAmount =
                refundRepository
                        .pendingRefundAmount(
                                tenantId(),
                                branchId
                        );

        return VatDashboardResponse.builder()
                .currentVatPosition(
                        payable.subtract(
                                credit
                        )
                )
                .outstandingVat(
                        payable
                )
                .availableCredit(
                        credit
                )
                .pendingRefundAmount(
                        pendingRefundAmount
                )
                .totalVatPaid(
                        totalPaid
                )
                .latestFilingId(
                        latestFiling != null
                                ? latestFiling.getId()
                                : null
                )
                .latestFiledPeriodStart(
                        latestFiling != null
                                ? latestFiling.getPeriod().getStartDate()
                                : null
                )
                .latestFiledPeriodEnd(
                        latestFiling != null
                                ? latestFiling.getPeriod().getEndDate()
                                : null
                )
                .latestFilingStatus(
                        latestFiling != null
                                ? VatStatusMapper.businessStatus(
                                latestFiling.getStatus()
                        )
                                : null
                )
                .nextUnfiledPeriodId(
                        nextUnfiled != null
                                ? nextUnfiled.getId()
                                : null
                )
                .nextUnfiledPeriodStart(
                        nextUnfiled != null
                                ? nextUnfiled.getStartDate()
                                : null
                )
                .nextUnfiledPeriodEnd(
                        nextUnfiled != null
                                ? nextUnfiled.getEndDate()
                                : null
                )
                .filingRequired(
                        nextUnfiled != null
                )
                .vatRate(
                        state.getVatRate()
                )
                .build();
    }

    private BigDecimal balance(
            UUID branchId,
            String role
    ) {

        UUID accountId =
                accounts.get(
                        tenantId(),
                        branchId,
                        role
                );

        return accountBalanceRepository
                .findByTenantIdAndAccount_IdAndBranch_Id(
                        tenantId(),
                        accountId,
                        branchId
                )
                .map(
                        AccountBalance::getBalance
                )
                .orElse(
                        BigDecimal.ZERO
                );
    }

    public VatOverviewResponse overview(
            UUID branchId
    ) {

        VatDashboardResponse dashboard =
                dashboard(
                        branchId
                );

        return VatOverviewResponse.builder()
                .vatToPay(
                        dashboard.outstandingVat()
                )
                .creditAvailable(
                        dashboard.availableCredit()
                )
                .refundsPending(
                        refundRepository
                                .countByTenantIdAndBranchIdAndStatus(
                                        tenantId(),
                                        branchId,
                                        VatRefundStatus.REQUESTED
                                )
                )
                .lastFilingId(
                        dashboard.latestFilingId()
                )
                .lastReturnStart(
                        dashboard.latestFiledPeriodStart()
                )
                .lastReturnEnd(
                        dashboard.latestFiledPeriodEnd()
                )
                .nextReturnPeriodId(
                        dashboard.nextUnfiledPeriodId()
                )
                .nextReturnStart(
                        dashboard.nextUnfiledPeriodStart()
                )
                .nextReturnEnd(
                        dashboard.nextUnfiledPeriodEnd()
                )
                .build();
    }
}