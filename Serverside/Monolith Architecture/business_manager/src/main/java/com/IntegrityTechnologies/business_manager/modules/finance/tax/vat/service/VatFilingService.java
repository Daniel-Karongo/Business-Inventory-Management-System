package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountBalance;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountBalanceRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.base.model.TaxSystemState;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.base.service.TaxSystemStateService;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.VatFiling;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.enums.VatFilingStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.repository.VatFilingRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class VatFilingService {

    private final VatReportService reportService;
    private final VatFilingRepository filingRepo;
    private final AccountingFacade accountingFacade;
    private final AccountingAccounts accounts;
    private final TaxSystemStateService taxSystemStateService;
    private final AccountBalanceRepository accountBalanceRepository;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    @Transactional
    public VatFiling file(
            AccountingPeriod period,
            UUID branchId,
            String user
    ) {

        if (branchId == null) {
            throw new IllegalArgumentException(
                    "BranchId required for VAT filing."
            );
        }

        if (
                filingRepo.existsByTenantIdAndPeriod_IdAndBranchId(
                        tenantId(),
                        period.getId(),
                        branchId
                )
        ) {
            throw new IllegalStateException(
                    "VAT already filed for this period and branch."
            );
        }

        if (!period.isClosed()) {
            throw new IllegalStateException(
                    "VAT filing allowed only after tax period is closed."
            );
        }

        if (period.getReopenedAt() != null) {
            throw new IllegalStateException(
                    "VAT filing prohibited for reopened periods."
            );
        }

        TaxSystemState state =
                taxSystemStateService.getOrCreate(
                        branchId
                );

        var report =
                reportService.generate(
                        period.getStartDate().atStartOfDay(),
                        period.getEndDate().atTime(
                                23,
                                59,
                                59
                        ),
                        branchId
                );

        BigDecimal outputVat =
                report.outputVat();

        BigDecimal inputVat =
                report.inputVat();

        BigDecimal currentPosition =
                outputVat.subtract(
                        inputVat
                );

        BigDecimal openingCredit =
                getCarryForwardBalance(
                        branchId
                );

        accountingFacade.post(
                AccountingEvent.builder()
                        .eventId(UUID.randomUUID())
                        .sourceId(period.getId())
                        .sourceModule("VAT_CLEARING")
                        .reference(
                                "VAT-CLEAR-"
                                        + period.getId()
                                        + "-"
                                        + branchId
                        )
                        .description(
                                "VAT clearing for period "
                                        + period.getStartDate()
                                        + " to "
                                        + period.getEndDate()
                        )
                        .performedBy(user)
                        .branchId(branchId)
                        .tenantId(tenantId())
                        .accountingDate(period.getEndDate())
                        .entries(
                                buildVatClearingEntries(
                                        branchId,
                                        outputVat,
                                        inputVat,
                                        currentPosition
                                )
                        )
                        .build()
        );

        BigDecimal creditApplied =
                openingCredit.min(
                        currentPosition.max(
                                BigDecimal.ZERO
                        )
                );

        if (creditApplied.compareTo(BigDecimal.ZERO) > 0) {

            accountingFacade.post(
                    AccountingEvent.builder()
                            .eventId(UUID.randomUUID())
                            .tenantId(tenantId())
                            .branchId(branchId)
                            .sourceModule("VAT_CARRY_FORWARD_CONSUMPTION")
                            .sourceId(period.getId())
                            .reference(
                                    "VAT-CF-CONSUME-"
                                            + period.getId()
                            )
                            .description(
                                    "Consume carried forward VAT credit"
                            )
                            .performedBy(user)
                            .accountingDate(period.getEndDate())
                            .entries(
                                    List.of(
                                            AccountingEvent.Entry.builder()
                                                    .accountId(
                                                            accounts.get(
                                                                    tenantId(),
                                                                    branchId,
                                                                    "VAT_RECEIVABLE"
                                                            )
                                                    )
                                                    .direction(
                                                            EntryDirection.DEBIT
                                                    )
                                                    .amount(
                                                            creditApplied
                                                    )
                                                    .build(),

                                            AccountingEvent.Entry.builder()
                                                    .accountId(
                                                            accounts.get(
                                                                    tenantId(),
                                                                    branchId,
                                                                    "VAT_CARRY_FORWARD"
                                                            )
                                                    )
                                                    .direction(
                                                            EntryDirection.CREDIT
                                                    )
                                                    .amount(
                                                            creditApplied
                                                    )
                                                    .build()
                                    )
                            )
                            .build()
            );
        }

        BigDecimal payable =
                currentPosition.subtract(
                        creditApplied
                );

        BigDecimal closingCredit =
                payable.compareTo(BigDecimal.ZERO) < 0
                        ? payable.abs()
                        : BigDecimal.ZERO;

        BigDecimal vatReceivableCreated =
                payable.compareTo(BigDecimal.ZERO) < 0
                        ? payable.abs()
                        : BigDecimal.ZERO;

        if (
                closingCredit.compareTo(
                        BigDecimal.ZERO
                ) > 0
        ) {

            accountingFacade.post(
                    AccountingEvent.builder()
                            .eventId(UUID.randomUUID())
                            .tenantId(tenantId())
                            .branchId(branchId)
                            .sourceModule("VAT_CARRY_FORWARD")
                            .sourceId(period.getId())
                            .reference(
                                    "VAT-CF-"
                                            + period.getId()
                            )
                            .description(
                                    "VAT credit carried forward"
                            )
                            .performedBy(user)
                            .accountingDate(
                                    period.getEndDate()
                            )
                            .entries(
                                    List.of(
                                            AccountingEvent.Entry.builder()
                                                    .accountId(
                                                            accounts.get(
                                                                    tenantId(),
                                                                    branchId,
                                                                    "VAT_CARRY_FORWARD"
                                                            )
                                                    )
                                                    .direction(
                                                            EntryDirection.DEBIT
                                                    )
                                                    .amount(
                                                            closingCredit
                                                    )
                                                    .build(),

                                            AccountingEvent.Entry.builder()
                                                    .accountId(
                                                            accounts.get(
                                                                    tenantId(),
                                                                    branchId,
                                                                    "VAT_RECEIVABLE"
                                                            )
                                                    )
                                                    .direction(
                                                            EntryDirection.CREDIT
                                                    )
                                                    .amount(
                                                            closingCredit
                                                    )
                                                    .build()
                                    )
                            )
                            .build()
            );
        }

        VatFilingStatus status;

        if (
                payable.compareTo(
                        BigDecimal.ZERO
                ) > 0
        ) {

            status =
                    VatFilingStatus.VAT_PAYABLE;

        } else if (
                closingCredit.compareTo(
                        BigDecimal.ZERO
                ) > 0
        ) {

            status =
                    VatFilingStatus.VAT_CREDIT_CARRIED_FORWARD;

        } else {

            status =
                    VatFilingStatus.PAID;
        }

        VatFiling filing =
                VatFiling.builder()
                        .period(period)
                        .branchId(branchId)
                        .tenantId(tenantId())
                        .outputVat(outputVat)
                        .inputVat(inputVat)
                        .vatPayable(payable)
                        .openingCredit(openingCredit)
                        .creditApplied(creditApplied)
                        .closingCredit(closingCredit)
                        .vatReceivableCreated(vatReceivableCreated)
                        .status(status)
                        .filedBy(user)
                        .filedAt(LocalDateTime.now())
                        .paid(false)
                        .paidAt(null)
                        .build();

        filingRepo.save(
                filing
        );

        return filing;
    }

    private BigDecimal getCarryForwardBalance(
            UUID branchId
    ) {

        UUID carryForwardAccountId =
                accounts.get(
                        tenantId(),
                        branchId,
                        "VAT_CARRY_FORWARD"
                );

        return accountBalanceRepository
                .findByTenantIdAndAccount_IdAndBranch_Id(
                        tenantId(),
                        carryForwardAccountId,
                        branchId
                )
                .map(AccountBalance::getBalance)
                .orElse(BigDecimal.ZERO);
    }

    private List<AccountingEvent.Entry> buildVatClearingEntries(
            UUID branchId,
            BigDecimal outputVat,
            BigDecimal inputVat,
            BigDecimal payable
    ) {

        List<AccountingEvent.Entry> entries =
                new ArrayList<>();

        if (
                outputVat.compareTo(
                        BigDecimal.ZERO
                ) > 0
        ) {

            entries.add(
                    AccountingEvent.Entry.builder()
                            .accountId(
                                    accounts.get(
                                            tenantId(),
                                            branchId,
                                            "VAT_OUTPUT"
                                    )
                            )
                            .direction(
                                    EntryDirection.DEBIT
                            )
                            .amount(
                                    outputVat
                            )
                            .build()
            );
        }

        if (
                inputVat.compareTo(
                        BigDecimal.ZERO
                ) > 0
        ) {

            entries.add(
                    AccountingEvent.Entry.builder()
                            .accountId(
                                    accounts.get(
                                            tenantId(),
                                            branchId,
                                            "VAT_INPUT"
                                    )
                            )
                            .direction(
                                    EntryDirection.CREDIT
                            )
                            .amount(
                                    inputVat
                            )
                            .build()
            );
        }

        if (
                payable.compareTo(
                        BigDecimal.ZERO
                ) > 0
        ) {

            entries.add(
                    AccountingEvent.Entry.builder()
                            .accountId(
                                    accounts.get(
                                            tenantId(),
                                            branchId,
                                            "VAT_PAYABLE"
                                    )
                            )
                            .direction(
                                    EntryDirection.CREDIT
                            )
                            .amount(
                                    payable
                            )
                            .build()
            );

        } else if (
                payable.compareTo(
                        BigDecimal.ZERO
                ) < 0
        ) {

            entries.add(
                    AccountingEvent.Entry.builder()
                            .accountId(
                                    accounts.get(
                                            tenantId(),
                                            branchId,
                                            "VAT_RECEIVABLE"
                                    )
                            )
                            .direction(
                                    EntryDirection.DEBIT
                            )
                            .amount(
                                    payable.abs()
                            )
                            .build()
            );
        }

        return entries;
    }

    @Transactional
    public void markPaid(
            VatFiling filing,
            String user,
            UUID paymentAccountId
    ) {

        if (
                filing.getVatPayable()
                        .compareTo(
                                BigDecimal.ZERO
                        ) <= 0
        ) {
            throw new IllegalStateException(
                    "No VAT payable exists."
            );
        }

        if (filing.isPaid()) {
            throw new IllegalStateException(
                    "VAT filing already paid and locked."
            );
        }

        UUID branchId =
                filing.getBranchId();

        BigDecimal amount =
                filing.getVatPayable();

        accountingFacade.post(
                AccountingEvent.builder()
                        .eventId(UUID.randomUUID())
                        .sourceId(filing.getId())
                        .sourceModule("VAT_PAYMENT")
                        .reference(
                                "VAT-PAY-"
                                        + filing.getId()
                        )
                        .description(
                                "VAT payment for period"
                        )
                        .performedBy(user)
                        .branchId(branchId)
                        .tenantId(tenantId())
                        .entries(
                                List.of(
                                        AccountingEvent.Entry.builder()
                                                .accountId(
                                                        accounts.get(
                                                                tenantId(),
                                                                branchId,
                                                                "VAT_PAYABLE"
                                                        )
                                                )
                                                .direction(
                                                        EntryDirection.DEBIT
                                                )
                                                .amount(
                                                        amount
                                                )
                                                .build(),

                                        AccountingEvent.Entry.builder()
                                                .accountId(
                                                        paymentAccountId
                                                )
                                                .direction(
                                                        EntryDirection.CREDIT
                                                )
                                                .amount(
                                                        amount
                                                )
                                                .build()
                                )
                        )
                        .build()
        );

        filing.setPaid(
                true
        );

        filing.setPaidAt(
                LocalDateTime.now()
        );

        filing.setStatus(
                VatFilingStatus.PAID
        );

        filingRepo.save(
                filing
        );
    }

    @Transactional
    public void requestRefund(
            UUID filingId,
            String user
    ) {

        VatFiling filing =
                filingRepo.findByTenantIdAndId(
                        tenantId(),
                        filingId
                ).orElseThrow(() ->
                        new IllegalArgumentException(
                                "VAT filing not found"
                        ));

        if (filing.getStatus() != VatFilingStatus.VAT_CREDIT_CARRIED_FORWARD)
        {
            throw new IllegalStateException(
                    "Refund request not allowed"
            );
        }

        filing.setStatus(
                VatFilingStatus.VAT_REFUND_PENDING
        );

        filingRepo.save(
                filing
        );
    }

    @Transactional
    public void completeRefund(
            UUID filingId,
            UUID receivingAccountId,
            String user
    ) {

        VatFiling filing =
                filingRepo.findByTenantIdAndId(
                        tenantId(),
                        filingId
                ).orElseThrow(() ->
                        new IllegalArgumentException(
                                "VAT filing not found"
                        ));

        if (
                filing.getStatus()
                        != VatFilingStatus.VAT_REFUND_PENDING
        ) {
            throw new IllegalStateException(
                    "VAT refund not pending"
            );
        }

        UUID branchId = filing.getBranchId();

        BigDecimal amount =
                filing.getClosingCredit();

        if (
                filing.getStatus()
                        != VatFilingStatus.VAT_REFUND_PENDING
        ) {
            throw new IllegalStateException(
                    "VAT refund not pending"
            );
        }

        if (
                filing.getClosingCredit()
                        .compareTo(BigDecimal.ZERO) <= 0
        ) {
            throw new IllegalStateException(
                    "No refundable credit exists"
            );
        }

        if (
                amount == null
                        || amount.compareTo(BigDecimal.ZERO) <= 0
        ) {
            throw new IllegalStateException(
                    "No refundable VAT credit exists"
            );
        }

        UUID carryForwardAccountId =
                accounts.get(
                        tenantId(),
                        branchId,
                        "VAT_CARRY_FORWARD"
                );

        BigDecimal carryForwardBalance =
                accountBalanceRepository
                        .findByTenantIdAndAccount_IdAndBranch_Id(
                                tenantId(),
                                carryForwardAccountId,
                                branchId
                        )
                        .map(AccountBalance::getBalance)
                        .orElse(BigDecimal.ZERO);

        if (amount.compareTo(carryForwardBalance) > 0) {
            throw new IllegalStateException(
                    "Refund exceeds VAT carry-forward balance"
            );
        }

        accountingFacade.post(
                AccountingEvent.builder()
                        .eventId(UUID.randomUUID())
                        .sourceId(filing.getId())
                        .sourceModule("VAT_REFUND")
                        .reference(
                                "VAT-REFUND-" + filing.getId()
                        )
                        .description(
                                "VAT refund received"
                        )
                        .performedBy(user)
                        .tenantId(tenantId())
                        .branchId(branchId)
                        .entries(
                                List.of(
                                        AccountingEvent.Entry.builder()
                                                .accountId(
                                                        receivingAccountId
                                                )
                                                .direction(
                                                        EntryDirection.DEBIT
                                                )
                                                .amount(
                                                        amount
                                                )
                                                .build(),

                                        AccountingEvent.Entry.builder()
                                                .accountId(
                                                        carryForwardAccountId
                                                )
                                                .direction(
                                                        EntryDirection.CREDIT
                                                )
                                                .amount(
                                                        amount
                                                )
                                                .build()
                                )
                        )
                        .build()
        );

        filing.setStatus(
                VatFilingStatus.VAT_REFUNDED
        );

        filingRepo.save(
                filing
        );
    }
}