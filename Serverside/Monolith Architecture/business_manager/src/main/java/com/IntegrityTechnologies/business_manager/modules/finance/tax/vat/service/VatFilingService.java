package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountBalance;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountBalanceRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.VatCreditMovement;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.VatFiling;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.VatPayment;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.VatRefund;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.enums.VatCreditMovementType;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.enums.VatFilingStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.enums.VatRefundStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.repository.VatCreditMovementRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.repository.VatFilingRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.repository.VatPaymentRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.repository.VatRefundRepository;
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
    private final VatPaymentRepository vatPaymentRepository;
    private final AccountingFacade accountingFacade;
    private final AccountingAccounts accounts;
    private final AccountBalanceRepository accountBalanceRepository;
    private final VatRefundRepository refundRepository;
    private final VatCreditMovementRepository creditMovementRepository;

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

        if (
                creditApplied.compareTo(
                        BigDecimal.ZERO
                ) > 0
        ) {

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
                                                                    branchId,"VAT_CARRY_FORWARD"
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
                            .accountingDate(period.getEndDate())
                            .entries(
                                    List.of(
                                            AccountingEvent.Entry.builder()
                                                    .accountId(
                                                            accounts.get(
                                                                    tenantId(),
                                                                    branchId,"VAT_CARRY_FORWARD"
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
                payable.compareTo(BigDecimal.ZERO) > 0
        ) {
            status = VatFilingStatus.VAT_PAYABLE;
        }
        else if (
                closingCredit.compareTo(BigDecimal.ZERO) > 0
        ) {
            status = VatFilingStatus.VAT_CREDIT_CARRIED_FORWARD;
        }
        else {
            status = VatFilingStatus.PAID;
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
                        .paidAmount(BigDecimal.ZERO)
                        .outstandingAmount(
                                payable.compareTo(BigDecimal.ZERO) > 0
                                        ? payable
                                        : BigDecimal.ZERO
                        )
                        .status(status)
                        .filedBy(user)
                        .filedAt(LocalDateTime.now())
                        .paidAt(null)
                        .build();

        filing = filingRepo.save(
                filing
        );

        if (
                creditApplied.compareTo(
                        BigDecimal.ZERO
                ) > 0
        ) {

            creditMovementRepository.save(
                    VatCreditMovement.builder()
                            .tenantId(
                                    tenantId()
                            )
                            .branchId(
                                    branchId
                            )
                            .filing(
                                    filing
                            )
                            .type(
                                    VatCreditMovementType.CONSUMED
                            )
                            .amount(
                                    creditApplied
                            )
                            .createdAt(
                                    LocalDateTime.now()
                            )
                            .build()
            );
        }

        if (
                closingCredit.compareTo(
                        BigDecimal.ZERO
                ) > 0
        ) {

            creditMovementRepository.save(
                    VatCreditMovement.builder()
                            .tenantId(
                                    tenantId()
                            )
                            .branchId(
                                    branchId
                            )
                            .filing(
                                    filing
                            )
                            .type(
                                    VatCreditMovementType.GENERATED
                            )
                            .amount(
                                    closingCredit
                            )
                            .createdAt(
                                    LocalDateTime.now()
                            )
                            .build()
            );
        }

        return filing;
    }


    @Transactional
    public void recordPayment(
            UUID filingId,
            BigDecimal amount,
            UUID fundingAccountId,
            String user
    ) {

        VatFiling filing =
                filingRepo
                        .findByTenantIdAndId(
                                tenantId(),
                                filingId
                        )
                        .orElseThrow(
                                () ->
                                        new IllegalArgumentException(
                                                "VAT filing not found"
                                        )
                        );

        if (
                amount == null
                        || amount.compareTo(BigDecimal.ZERO) <= 0
        ) {
            throw new IllegalArgumentException(
                    "Payment amount must be greater than zero"
            );
        }

        if (
                filing.getOutstandingAmount()
                        .compareTo(BigDecimal.ZERO) <= 0
        ) {
            throw new IllegalStateException(
                    "VAT already fully paid"
            );
        }

        if (
                amount.compareTo(
                        filing.getOutstandingAmount()
                ) > 0
        ) {
            throw new IllegalStateException(
                    "Payment exceeds outstanding balance"
            );
        }

        UUID branchId =
                filing.getBranchId();

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
                                "VAT payment"
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
                                                        fundingAccountId
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

        VatPayment payment =
                VatPayment.builder()
                        .tenantId(
                                tenantId()
                        )
                        .branchId(
                                branchId
                        )
                        .filing(
                                filing
                        )
                        .amount(
                                amount
                        )
                        .fundingAccountId(
                                fundingAccountId
                        )
                        .recordedBy(
                                user
                        )
                        .recordedAt(
                                LocalDateTime.now()
                        )
                        .build();

        vatPaymentRepository.save(
                payment
        );

        BigDecimal newPaidAmount =
                filing.getPaidAmount()
                        .add(amount);

        BigDecimal newOutstanding =
                filing.getOutstandingAmount()
                        .subtract(amount);

        filing.setPaidAmount(
                newPaidAmount
        );

        filing.setOutstandingAmount(
                newOutstanding
        );

        if (
                newOutstanding.compareTo(
                        BigDecimal.ZERO
                ) == 0
        ) {

            filing.setStatus(
                    VatFilingStatus.PAID
            );

            filing.setPaidAt(
                    LocalDateTime.now()
            );
        }
        else {

            filing.setStatus(
                    VatFilingStatus.PARTIALLY_PAID
            );
        }

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
                filingRepo
                        .findByTenantIdAndId(
                                tenantId(),
                                filingId
                        )
                        .orElseThrow(
                                () ->
                                        new IllegalArgumentException(
                                                "VAT filing not found"
                                        )
                        );

        if (
                filing.getStatus()
                        != VatFilingStatus.VAT_CREDIT_CARRIED_FORWARD
        ) {
            throw new IllegalStateException(
                    "Refund request not allowed"
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

        filing.setStatus(
                VatFilingStatus.VAT_REFUND_PENDING
        );

        refundRepository.save(
                VatRefund.builder()
                        .tenantId(
                                tenantId()
                        )
                        .branchId(
                                filing.getBranchId()
                        )
                        .filing(
                                filing
                        )
                        .amount(
                                filing.getClosingCredit()
                        )
                        .status(
                                VatRefundStatus.REQUESTED
                        )
                        .requestedBy(
                                user
                        )
                        .requestedAt(
                                LocalDateTime.now()
                        )
                        .build()
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
                filingRepo
                        .findByTenantIdAndId(
                                tenantId(),
                                filingId
                        )
                        .orElseThrow(
                                () ->
                                        new IllegalArgumentException(
                                                "VAT filing not found"
                                        )
                        );

        if (
                filing.getStatus()
                        != VatFilingStatus.VAT_REFUND_PENDING
        ) {
            throw new IllegalStateException(
                    "VAT refund not pending"
            );
        }

        UUID branchId =
                filing.getBranchId();

        BigDecimal amount =
                filing.getClosingCredit();

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
                        .findByTenantIdAndAccount_IdAndBranchId(
                                tenantId(),
                                carryForwardAccountId,
                                branchId
                        )
                        .map(AccountBalance::getBalance)
                        .orElse(BigDecimal.ZERO);

        if (
                amount.compareTo(
                        carryForwardBalance
                ) > 0
        ) {
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
                                "VAT-REFUND-"
                                        + filing.getId()
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

        VatRefund refund =
                refundRepository
                        .findByTenantIdAndFiling_Id(
                                tenantId(),
                                filing.getId()
                        )
                        .orElseThrow();

        refund.setStatus(
                VatRefundStatus.COMPLETED
        );

        refund.setProcessedBy(
                user
        );

        refund.setProcessedAt(
                LocalDateTime.now()
        );

        refundRepository.save(
                refund
        );

        filing.setStatus(
                VatFilingStatus.VAT_REFUNDED
        );

        creditMovementRepository.save(
                VatCreditMovement.builder()
                        .tenantId(
                                tenantId()
                        )
                        .branchId(
                                branchId
                        )
                        .filing(
                                filing
                        )
                        .type(
                                VatCreditMovementType.REFUNDED
                        )
                        .amount(
                                amount
                        )
                        .createdAt(
                                LocalDateTime.now()
                        )
                        .build()
        );

        filingRepo.save(
                filing
        );
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
                .findByTenantIdAndAccount_IdAndBranchId(
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
        }
        else if (
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
}